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
import System.Posix.Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.Semigroup
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
import Options.Applicative
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Format as Time


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

import qualified Language.Elm.LSP.Diagnostics as Diagnostics

data CommandLineOptions
    = CommandLineOptions
    { serverLogFile :: FilePath
    , sessionLogFile :: FilePath
    }

commandLineOptionsParser :: String -> Parser CommandLineOptions
commandLineOptionsParser logSuffix = CommandLineOptions
    <$> strOption
        ( long "server-log-file"
        <> metavar "FILENAME"
        <> help "Log file used for general server logging"
        <> value ("/tmp/elm-language-server-" ++ logSuffix ++ ".log")
        <> showDefault
        )
    <*> strOption
        ( long "session-log-file"
        <> metavar "FILENAME"
        <> help "Log file used for general server logging"
        <> value ("/tmp/elm-language-session-" ++ logSuffix ++ ".log")
        <> showDefault
        )

commandLineOptions :: String -> ParserInfo CommandLineOptions
commandLineOptions logSuffix = info (commandLineOptionsParser logSuffix <**> helper)
    ( fullDesc
    <> header "elm-language-server"
    <> progDesc "A Language Server Protocol Implementation for the Elm Language (see https://elm-lang.org)"
    )

main :: IO ()
main = do
    zonedTime <- Time.getZonedTime
    let isoFormat = Time.iso8601DateFormat (Just "%H:%M:%S")

    opts <- execParser (commandLineOptions (Time.formatTime Time.defaultTimeLocale isoFormat zonedTime))
    run opts (return ()) >>= \case
        0 -> exitSuccess
        c -> exitWith . ExitFailure $ c

run :: CommandLineOptions -> IO () -> IO Int
run opts dispatcherProc = flip Exception.catches handlers $ do
    rin <- atomically newTChan :: IO (TChan ReactorInput)
    let dp lf = do
            _rpid <- forkIO $ reactor lf rin
            dispatcherProc
            return Nothing
    flip Exception.finally finalProc $ do
        LSP.Core.setupLogger (Just (serverLogFile opts)) [] L.DEBUG
        LSP.Control.run
            (return (Right ()), dp)
            (lspHandlers rin)
            lspOptions
            (Just (sessionLogFile opts))
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

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: LSP.Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp =
    flip runReaderT lf $ forever $ do
        inval <- liftIO $ atomically $ readTChan inp
        case inval of
            HandlerRequest (RspFromClient rm) ->
                liftIO $ LSP.logs $ "reactor:got RspFromClient:" ++ show rm

            HandlerRequest (NotInitialized _notification) ->
                Diagnostics.typeCheckAndReportDiagnostics

            HandlerRequest (NotDidSaveTextDocument notification ) -> do
                let fileUri  = notification ^. LSP.params . LSP.textDocument . LSP.uri
                let filePath = LSP.uriToFilePath fileUri
                lf <- ask
                liftIO $ LSP.Core.flushDiagnosticsBySourceFunc lf 200 (Just "elm-language-server")
                Diagnostics.typeCheckAndReportDiagnostics

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
passHandler rin c notification =
    atomically $ writeTChan rin (HandlerRequest (c notification))
