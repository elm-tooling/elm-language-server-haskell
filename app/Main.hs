{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.Aeson as Json
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import System.Posix.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Lens as LSP
import qualified Language.Haskell.LSP.Utility as LSP
import Language.Haskell.LSP.VFS
import System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi

-- ELM COMPILER MODULES
import qualified Elm.Compiler
import qualified Elm.Compiler.Module
import qualified Elm.Package
import qualified Elm.Project.Json
import qualified File.Args
import qualified File.Compile
import qualified File.Crawl
import qualified File.Plan
import qualified Reporting.Progress.Json
import qualified Reporting.Task
import qualified Reporting.Doc
import qualified Reporting.Error
import qualified Reporting.Render.Code
import qualified Reporting.Report
import qualified "elm" Reporting.Region -- would conflict with elm-format's Reporting.Region
import qualified Stuff.Verify


main :: IO ()
main = do
    run (return ()) >>= \case
        0 -> exitSuccess
        c -> exitWith . ExitFailure $ c

run :: IO () -> IO Int
run dispatcherProc = flip Exception.catches handlers $ do
    rin <- atomically newTChan :: IO (TChan ReactorInput)
    let dp lf = do
            _rpid <- forkIO $ reactor lf rin
            dispatcherProc
            return Nothing
    flip Exception.finally finalProc $ do
        pid <- getProcessID
        --(Just ("/tmp/elm-language-server-" ++ show pid ++ ".log")) TODO: Reintroduce pid/date logfiles + add program option for log file destination
        LSP.Core.setupLogger (Just ("/tmp/elm-language-server.log")) [] L.DEBUG
        LSP.Control.run (return (Right ()), dp) (lspHandlers rin) lspOptions (Just ("/tmp/elm-language-session.log"))
  where
    handlers  = [Exception.Handler ioExcept, Exception.Handler someExcept]
    finalProc = L.removeAllHandlers
    ioExcept (e :: Exception.IOException) = print e >> return 1
    someExcept (e :: Exception.SomeException) = print e >> return 1

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
data ReactorInput =
  HandlerRequest FromClientMessage -- ^ injected into the reactor input by each of the individual callback handlers

-- | The monad used in the reactor
type R c a = ReaderT (LSP.Core.LspFuncs c) IO a

publishDiagnostics :: Int -> LSP.Uri -> LSP.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
    lf <- ask
    liftIO $ (LSP.Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: LSP.Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp = do
    flip runReaderT lf $ forever $ do
        inval <- liftIO $ atomically $ readTChan inp
        case inval of
            HandlerRequest (RspFromClient rm) -> do
                liftIO $ LSP.logs $ "reactor:got RspFromClient:" ++ show rm

            HandlerRequest (NotInitialized _notification) ->
                compileAndReportDiagnostics Nothing

            HandlerRequest (NotDidSaveTextDocument notification ) -> do
                let fileUri  = notification ^. LSP.params . LSP.textDocument . LSP.uri
                let filePath = LSP.uriToFilePath fileUri
                LSP.Core.flushDiagnosticsBySourceFunc <$> ask
                -- we can't use (fmap return filePath) in here, because we need to update dependents also
                -- seems like we need to look backwards in the dependency graph ourself
                compileAndReportDiagnostics Nothing

compileAndReportDiagnostics :: Maybe [FilePath] -> R () ()
compileAndReportDiagnostics maybeOpenedFile = do
    lf <- ask
    case LSP.Core.rootPath lf of
        Nothing -> liftIO $ LSP.logm "NO ROOTPATH"

        Just root -> do
            answers <- liftIO $ compileFiles root maybeOpenedFile
            reportAnswers answers

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: FilePath -> Reporting.Report.Report -> R () ()
sendDiagnostics filePath (Reporting.Report.Report title region suggestions messageDoc) = do
    let translatePosition (Reporting.Region.Position line column) = LSP.Position (line - 1) (column - 1) -- elm uses 1-based indices (for being human friendly)
    let (Reporting.Region.Region start end) = region
    let fileUri = LSP.filePathToUri filePath
    let diags =
            [ LSP.Diagnostic
                (LSP.Range (translatePosition start) (translatePosition end))
                (Just LSP.DsError) -- severity
                Nothing -- code
                (Just "elm-language-server") -- source
                (T.pack (Reporting.Doc.toString messageDoc))
                (Just (LSP.List []))
            ]
    publishDiagnostics 100 fileUri (Just 0) (partitionBySource diags)

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions = LSP.TextDocumentSyncOptions
    { LSP._openClose = Just False
    , LSP._change = Just LSP.TdSyncNone
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.SaveOptions $ Just False
    }

lspOptions :: LSP.Core.Options
lspOptions = def
    { LSP.Core.textDocumentSync = Just syncOptions }

lspHandlers :: TChan ReactorInput -> LSP.Core.Handlers
lspHandlers rin = def 
    { LSP.Core.initializedHandler = Just $ passHandler rin NotInitialized
    , LSP.Core.didSaveTextDocumentNotificationHandler = Just $ passHandler rin NotDidSaveTextDocument
    }

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> LSP.Core.Handler a
passHandler rin c notification = do
    atomically $ writeTChan rin (HandlerRequest (c notification))

-- Use Elm Compiler to compiler files
-- Arguments are root (where elm.json lives) and filenames (or [])
compileFiles :: MonadIO m => String -> Maybe [FilePath] -> m (Maybe (Map.Map Elm.Compiler.Module.Raw File.Compile.Answer))
compileFiles root files = do
    liftIO $ Reporting.Task.try Reporting.Progress.Json.reporter $ do
        project <- Elm.Project.Json.read (root </> "elm.json")
        Elm.Project.Json.check project
        summary     <- Stuff.Verify.verify root project
        -- If no files are given, get files from the  project
        actualFiles <- case files of
            Nothing -> liftIO $ getElmFiles project
            Just f  -> return f
        args            <- File.Args.fromPaths summary actualFiles
        graph           <- File.Crawl.crawl summary args
        (dirty, ifaces) <- File.Plan.plan Nothing summary graph
        File.Compile.compile project Nothing ifaces dirty

filterBadAnswers :: [File.Compile.Answer] -> [(FilePath, [Elm.Compiler.Error])]
filterBadAnswers = mapMaybe $ \case
    File.Compile.Bad path _ _ errors -> Just (path, errors)
    _ -> Nothing

reportAnswers :: Maybe (Map.Map Elm.Compiler.Module.Raw File.Compile.Answer) -> R () ()
reportAnswers answers = do
    let answerList = map snd $ Map.toList (fromMaybe Map.empty answers)
    let badAnswers = filterBadAnswers answerList
    maybeRootPath <- LSP.Core.rootPath <$> ask
    case maybeRootPath of
        Nothing -> liftIO $ LSP.logm "NO ROOT PATH"

        Just rootPath -> do
            liftIO $ LSP.logs ("# of modules with errors: " ++ show (length badAnswers))
            forM_ badAnswers $ \(path, errors) -> do
                fileContent <- liftIO $ T.readFile path -- TODO: this is extremely inefficient
                let reports = concatMap (Reporting.Error.toReports (Reporting.Render.Code.toSource fileContent)) errors
                forM_ reports $ \report -> sendDiagnostics (rootPath </> path) report

-- Get all elm files given in an elm.json ([] for a package, all elm files for an application)
getElmFiles :: Elm.Project.Json.Project -> IO [FilePath]
getElmFiles summary = case summary of
    Elm.Project.Json.App app -> do
        let dirs = Elm.Project.Json._app_source_dirs app
        elmFiles <- mapM (Glob.globDir1 (Glob.compile "**/*.elm")) dirs
        return (concat elmFiles)
    Elm.Project.Json.Pkg package -> return []
