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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Maybe

import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.MessageFuncs as LSP
import qualified Language.Haskell.LSP.Types.Lens as LSP
import qualified Language.Haskell.LSP.Utility as LSP
import Language.Haskell.LSP.VFS

-- ELM COMPILER MODULES
import qualified Elm.Compiler
import qualified Elm.Compiler.Module
import qualified Elm.Interface
import qualified Elm.Package
import qualified Elm.Project.Json
import qualified Elm.Name
import qualified AST.Optimized
import qualified File.Args
import qualified File.Compile
import qualified File.Crawl
import qualified File.Plan
import qualified Reporting.Progress.Json
import qualified Reporting.Progress.Terminal
import qualified Reporting.Error
import qualified Reporting.Render.Code
import qualified Reporting.Report
import qualified Reporting.Progress
import qualified Reporting.Task
import qualified Reporting.Warning
import qualified Reporting.Result
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Doc
import qualified Reporting.Exit
import qualified "elm" Reporting.Region -- would conflict with elm-format's Reporting.Region
import qualified Stuff.Verify

import qualified Language.Elm as Elm

-- | The monad used in the reactor
type R c a = ReaderT (LSP.Core.LspFuncs c) IO a

publishDiagnostics :: Int -> LSP.Uri -> LSP.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
    lf <- ask
    liftIO $ LSP.Core.publishDiagnosticsFunc lf maxToPublish uri v diags

showMessageNotification :: LSP.MessageType -> T.Text -> R () ()
showMessageNotification messageType text = do
    lf <- ask
    liftIO $ LSP.Core.sendFunc lf (NotShowMessage (LSP.fmServerShowMessageNotification messageType text))

typeCheckAndReportDiagnostics :: R () ()
typeCheckAndReportDiagnostics = do
    lf <- ask
    case LSP.Core.rootPath lf of
        Nothing -> 
            liftIO $ LSP.logm "NO ROOTPATH"

        Just root -> do
            result <- liftIO $ Elm.typeCheckFiles root
            case result of
                Right answers ->
                    reportAnswers root answers

                Left exit ->
                    showMessageNotification LSP.MtError (T.pack (Reporting.Exit.toString exit))

sendReportAsDiagnostics :: FilePath -> Reporting.Report.Report -> R () ()
sendReportAsDiagnostics filePath report = do
    let diags = [reportToDiagnostic report]
    let fileUri = LSP.filePathToUri filePath
    publishDiagnostics 100 fileUri (Just 0) (partitionBySource diags)

reportToDiagnostic :: Reporting.Report.Report -> LSP.Diagnostic
reportToDiagnostic (Reporting.Report.Report title region suggestions messageDoc) =
    let 
        translatePosition (Reporting.Region.Position line column) = LSP.Position (line - 1) (column - 1) -- elm uses 1-based indices (for being human friendly)
        (Reporting.Region.Region start end) = region
    in
        LSP.Diagnostic
            (LSP.Range (translatePosition start) (translatePosition end))
            (Just LSP.DsError) -- severity
            Nothing -- code
            (Just "ElmLS") -- source
            (T.pack (Reporting.Doc.toString messageDoc)) -- TODO: The messageDoc also shows the source code, which is not necessary for diagnostics
            (Just (LSP.List []))

reportAnswers :: FilePath -> Elm.CheckingAnswers -> R () ()
reportAnswers rootPath checkingAnswers = do
    Map.traverseWithKey reportModule checkingAnswers
    return ()
  where
    reportModule moduleName result =
        case result of
            Left (Elm.TypeCheckFailure path errors) ->
                let
                    fileUri = LSP.filePathToUri (rootPath </> path)
                    diagnostics = map reportToDiagnostic errors
                in
                    publishDiagnostics 200 fileUri Nothing (partitionBySource diagnostics)

            Right _ -> return ()
