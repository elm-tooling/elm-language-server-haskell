{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.Default
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import System.Posix.Process
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Control as CTRL
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U
import Language.Haskell.LSP.VFS
import System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi

-- ELM COMPILER MODULES
import qualified Elm.Compiler.Module
import qualified Elm.Package
import qualified Elm.Project.Json
import qualified File.Args
import qualified File.Compile
import qualified File.Crawl
import qualified File.Plan
import qualified Reporting.Progress.Json
import qualified Reporting.Task
import qualified Stuff.Verify

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- ---------------------------------------------------------------------
--
main :: IO ()
main = do
  run (return ()) >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------
run :: IO () -> IO Int
run dispatcherProc =
  flip E.catches handlers $ do
    rin <- atomically newTChan :: IO (TChan ReactorInput)
    let dp lf = do
          liftIO $ U.logs "main.run:dp entered"
          _rpid <- forkIO $ reactor lf rin
          liftIO $ U.logs "main.run:dp tchan"
          dispatcherProc
          liftIO $ U.logs "main.run:dp after dispatcherProc"
          return Nothing
    flip E.finally finalProc $ do
      pid <- getProcessID
      Core.setupLogger
        (Just ("/tmp/elm-language-server-" ++ show pid ++ ".log"))
        []
        L.DEBUG
      CTRL.run
        (return (Right ()), dp)
        (lspHandlers rin)
        lspOptions
        (Just ("/tmp/elm-language-session-" ++ show pid ++ ".log"))
  where
    handlers = [E.Handler ioExcept, E.Handler someExcept]
    finalProc = L.removeAllHandlers
    ioExcept (e :: E.IOException) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1

-- ---------------------------------------------------------------------
-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
data ReactorInput =
  HandlerRequest FromClientMessage -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------
-- | The monad used in the reactor
type R c a = ReaderT (Core.LspFuncs c) IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
reactorSend :: FromServerMessage -> R () ()
reactorSend msg = do
  lf <- ask
  liftIO $ Core.sendFunc lf msg

-- ---------------------------------------------------------------------
publishDiagnostics ::
     Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

-- ---------------------------------------------------------------------
nextLspReqId :: R () J.LspId
nextLspReqId = do
  f <- asks Core.getNextReqId
  liftIO f

-- ---------------------------------------------------------------------
-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp = do
  liftIO $ U.logs "reactor:entered"
  flip runReaderT lf $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval
      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
          of
      HandlerRequest (RspFromClient rm) -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm
      -- -------------------------------
      HandlerRequest (NotInitialized _notification) -> do
        liftIO $ U.logm "****** reactor: processing Initialized Notification"
        -- Read project file
        lf <- ask
        liftIO $
          case Core.rootPath lf of
            Nothing -> U.logm "NO ROOTPATH"
            Just root -> do
              answers <- compileFiles root Nothing
              return ()
      -- -------------------------------
      HandlerRequest (NotDidOpenTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
        let doc = notification ^. J.params . J.textDocument . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc (Just 0)
      -- -------------------------------
      HandlerRequest (NotDidChangeTextDocument notification) -> do
        let doc :: J.Uri
            doc = notification ^. J.params . J.textDocument . J.uri
        mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
        case mdoc of
          Just (VirtualFile _version str) -> do
            liftIO $ U.logs $
              "reactor:processing NotDidChangeTextDocument: vf got:" ++
              (show $ Yi.toString str)
          Nothing -> do
            liftIO $
              U.logs
                "reactor:processing NotDidChangeTextDocument: vf returned Nothing"
        liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: uri=" ++
          (show doc)
      -- -------------------------------
      HandlerRequest (NotDidSaveTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let doc = notification ^. J.params . J.textDocument . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc Nothing
      -- -------------------------------
      HandlerRequest (ReqRename req) -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let _params = req ^. J.params
            _doc = _params ^. J.textDocument . J.uri
            J.Position _l _c' = _params ^. J.position
            _newName = _params ^. J.newName
        let we =
              J.WorkspaceEdit
                Nothing -- "changes" field is deprecated
                (Just (J.List [])) -- populate with actual changes from the rename
        let rspMsg = Core.makeResponseMessage req we
        reactorSend $ RspRename rspMsg
      -- -------------------------------
      HandlerRequest (ReqHover req) -> do
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let J.TextDocumentPositionParams _doc pos = req ^. J.params
            J.Position _l _c' = pos
        let ht = J.Hover ms (Just range)
            ms =
              J.List [J.CodeString $ J.LanguageString "lsp-hello" "TYPE INFO"]
            range = J.Range pos pos
        reactorSend $ RspHover $ Core.makeResponseMessage req ht
      -- -------------------------------
      HandlerRequest (ReqCodeAction req) -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            (J.List diags) = params ^. J.context . J.diagnostics
          -- makeCommand only generates commands for diagnostics whose source is us
        let makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m _l) =
              [J.Command title cmd cmdparams]
              where
                title = "Apply LSP hello command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
                cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
                args =
                  J.List
                    [ J.Object $
                      H.fromList
                        [ ( "file"
                          , J.Object $
                            H.fromList [("textDocument", J.toJSON doc)])
                        ]
                    , J.Object $
                      H.fromList
                        [ ( "start_pos"
                          , J.Object $ H.fromList [("position", J.toJSON start)])
                        ]
                    ]
                cmdparams = Just args
            makeCommand (J.Diagnostic _r _s _c _source _m _l) = []
        let body = J.List $ map J.CACommand $ concatMap makeCommand diags
            rsp = Core.makeResponseMessage req body
        reactorSend $ RspCodeAction rsp
      -- -------------------------------
      HandlerRequest (ReqExecuteCommand req) -> do
        liftIO $ U.logs "reactor:got ExecuteCommandRequest:" -- ++ show req
        let params = req ^. J.params
            margs = params ^. J.arguments
        liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
        let reply v =
              reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
            r = J.List [] :: J.List Int
        liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            reply (J.Object mempty)
            lid <- nextLspReqId
            -- reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            reactorSend $ ReqApplyWorkspaceEdit $
              fmServerApplyWorkspaceEditRequest lid we
          Nothing -> reply (J.Object mempty)
      -- -------------------------------
      HandlerRequest om -> do
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------
toWorkspaceEdit :: t -> Maybe J.ApplyWorkspaceEditParams
toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------
-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.Uri -> Maybe Int -> R () ()
sendDiagnostics fileUri version = do
  let diags =
        [ J.Diagnostic
            (J.Range (J.Position 0 1) (J.Position 0 5))
            (Just J.DsWarning) -- severity
            Nothing -- code
            (Just "lsp-hello") -- source
            "Example diagnostic message"
            (Just (J.List []))
        ]
  -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------
syncOptions :: J.TextDocumentSyncOptions
syncOptions =
  J.TextDocumentSyncOptions
    { J._openClose = Just False
    , J._change = Just J.TdSyncFull
    , J._willSave = Just False
    , J._willSaveWaitUntil = Just False
    , J._save = Just $ J.SaveOptions $ Just False
    }

lspOptions :: Core.Options
lspOptions = def {Core.textDocumentSync = Just syncOptions}

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin =
  def {Core.initializedHandler = Just $ passHandler rin NotInitialized}

-- ---------------------------------------------------------------------
passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

-- ---------------------------------------------------------------------
responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- Use Elm Compiler to compiler files
-- Arguments are root (where elm.json lives) and filenames (or [])
compileFiles root files = do
  liftIO $ Reporting.Task.try Reporting.Progress.Json.reporter $ do
    liftIO $ U.logs ("COMPILE1 " ++ root)
    project <- Elm.Project.Json.read (root </> "elm.json")
    liftIO $
      U.logs
        ("COMPILE2 " ++
         T.unpack (Elm.Package.toText (Elm.Project.Json.getName project)))
    Elm.Project.Json.check project
    liftIO $ U.logs ("COMPILE3 " ++ root)
    summary <- Stuff.Verify.verify root project
    -- If no files are given, get files from the  project
    actualFiles <-
      case files of
        Nothing -> liftIO $ getElmFiles project
        Just f -> return f
    liftIO $ U.logs ("COMPILE4 " ++ show actualFiles)
    args <- File.Args.fromPaths summary actualFiles
    liftIO $ U.logs ("COMPILE5 " ++ root)
    graph <- File.Crawl.crawl summary args
    liftIO $ U.logs ("COMPILE6 " ++ root)
    (dirty, ifaces) <- File.Plan.plan Nothing summary graph
    liftIO $ U.logs ("COMPILE7 " ++ root)
    File.Compile.compile project Nothing ifaces dirty

-- ---------------------------------------------------------------------
reportAnswers ::
     Maybe (Map.Map Elm.Compiler.Module.Raw File.Compile.Answer) -> R () ()
reportAnswers Nothing = return ()
reportAnswers (Just map) = do
  let list = Map.toList map
  sendDiagnostics
    (J.Uri $ T.pack $ Elm.Compiler.Module.nameToString (fst (head list)))
    (Just 0)

-- Get all elm files given in an elm.json ([] for a package, all elm files for an application)
getElmFiles :: Elm.Project.Json.Project -> IO [FilePath]
getElmFiles summary =
  case summary of
    Elm.Project.Json.App app -> do
      let dirs = Elm.Project.Json._app_source_dirs app
      elmFiles <- mapM (Glob.globDir1 (Glob.compile "**/*.elm")) dirs
      return (concat elmFiles)
    Elm.Project.Json.Pkg package -> return []
