{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

module Language.Elm.LSP.Diagnostics where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
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

-- | The monad used in the reactor
type R c a = ReaderT (LSP.Core.LspFuncs c) IO a

publishDiagnostics :: Int -> LSP.Uri -> LSP.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
    lf <- ask
    liftIO $ (LSP.Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

compileAndReportDiagnostics :: Maybe [FilePath] -> R () ()
compileAndReportDiagnostics maybeOpenedFile = do
    lf <- ask
    case LSP.Core.rootPath lf of
        Nothing -> liftIO $ LSP.logm "NO ROOTPATH"

        Just root -> do
            answers <- liftIO $ compileFiles root maybeOpenedFile
            reportAnswers root answers

sendReportAsDiagnostics :: FilePath -> Reporting.Report.Report -> R () ()
sendReportAsDiagnostics filePath (Reporting.Report.Report title region suggestions messageDoc) = do
    -- TODO: The messageDoc also shows the source code, which is not necessary for diagnostics
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

-- Use Elm Compiler to compiler files
-- Arguments are root (where elm.json lives) and filenames (or [])
compileFiles :: MonadIO m => String -> Maybe [FilePath] -> m (Maybe (Map.Map Elm.Compiler.Module.Raw File.Compile.Answer))
compileFiles root files = do
    liftIO $ Reporting.Task.try Reporting.Progress.Json.reporter $ do
        project <- Elm.Project.Json.read (root </> "elm.json")
        Elm.Project.Json.check project
        summary <- Stuff.Verify.verify root project
        -- If no files are given, get files from the  project
        actualFiles <- case files of
            Nothing -> liftIO $ getElmFiles project
            Just f  -> return f
        args <- File.Args.fromPaths summary actualFiles
        graph <- File.Crawl.crawl summary args
        (dirty, ifaces) <- File.Plan.plan Nothing summary graph
        File.Compile.compile project Nothing ifaces dirty

filterBadAnswers :: [File.Compile.Answer] -> [(FilePath, [Elm.Compiler.Error])]
filterBadAnswers = mapMaybe $ \case
    File.Compile.Bad path _ _ errors -> Just (path, errors)
    _ -> Nothing

reportAnswers :: FilePath -> Maybe (Map.Map Elm.Compiler.Module.Raw File.Compile.Answer) -> R () ()
reportAnswers rootPath answers = do
    let answerList = map snd $ Map.toList (fromMaybe Map.empty answers)
    let badAnswers = filterBadAnswers answerList
    forM_ badAnswers $ \(path, errors) -> do
        fileContent <- liftIO $ T.readFile path -- TODO: this is extremely inefficient
        let reports = concatMap (Reporting.Error.toReports (Reporting.Render.Code.toSource fileContent)) errors
        forM_ reports $ \report ->
            sendReportAsDiagnostics (rootPath </> path) report

-- Get all elm files given in an elm.json ([] for a package, all elm files for an application)
getElmFiles :: Elm.Project.Json.Project -> IO [FilePath]
getElmFiles summary = case summary of
    Elm.Project.Json.App app -> do
        let dirs = Elm.Project.Json._app_source_dirs app
        elmFiles <- mapM (Glob.globDir1 (Glob.compile "**/*.elm")) dirs
        return (concat elmFiles)
    Elm.Project.Json.Pkg package -> return []
