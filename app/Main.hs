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
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import qualified Data.Text as T
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
  flip Exception.catches handlers $ do
    rin <- atomically newTChan :: IO (TChan ReactorInput)
    let dp lf = do
          liftIO $ LSP.logs "main.run:dp entered"
          _rpid <- forkIO $ reactor lf rin
          liftIO $ LSP.logs "main.run:dp tchan"
          dispatcherProc
          liftIO $ LSP.logs "main.run:dp after dispatcherProc"
          return Nothing
    flip Exception.finally finalProc $ do
      pid <- getProcessID
      LSP.Core.setupLogger
        (Just ("/tmp/elm-language-server-" ++ show pid ++ ".log"))
        []
        L.DEBUG
      LSP.Control.run
        (return (Right ()), dp)
        (lspHandlers rin)
        lspOptions
        (Just ("/tmp/elm-language-session-" ++ show pid ++ ".log"))
  where
    handlers = [Exception.Handler ioExcept, Exception.Handler someExcept]
    finalProc = L.removeAllHandlers
    ioExcept (e :: Exception.IOException) = print e >> return 1
    someExcept (e :: Exception.SomeException) = print e >> return 1

-- ---------------------------------------------------------------------
-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
data ReactorInput =
  HandlerRequest FromClientMessage -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------
-- | The monad used in the reactor
type R c a = ReaderT (LSP.Core.LspFuncs c) IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
reactorSend :: FromServerMessage -> R () ()
reactorSend msg = do
  lf <- ask
  liftIO $ LSP.Core.sendFunc lf msg

-- ---------------------------------------------------------------------
publishDiagnostics ::
     Int -> LSP.Uri -> LSP.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  liftIO $ (LSP.Core.publishDiagnosticsFunc lf) maxToPublish uri v diags

-- ---------------------------------------------------------------------
nextLspReqId :: R () LSP.LspId
nextLspReqId = do
  f <- asks LSP.Core.getNextReqId
  liftIO f

-- ---------------------------------------------------------------------
-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: LSP.Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp = do
  liftIO $ LSP.logs "reactor:entered"
  flip runReaderT lf $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval
      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
          of
      HandlerRequest (RspFromClient rm) -> do
        liftIO $ LSP.logs $ "reactor:got RspFromClient:" ++ show rm
      -- -------------------------------
      HandlerRequest (NotInitialized _notification) -> do
        liftIO $ LSP.logm "****** reactor: processing Initialized Notification"
        -- Read project file
        lf <- ask
        liftIO $
          case LSP.Core.rootPath lf of
            Nothing -> LSP.logm "NO ROOTPATH"
            Just root -> do
              answers <- compileFiles root Nothing
              return ()
      -- -------------------------------
      HandlerRequest (NotDidOpenTextDocument notification) -> do
        liftIO $ LSP.logm "****** reactor: processing NotDidOpenTextDocument"
        let doc = notification ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        liftIO $ LSP.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc (Just 0)
      -- -------------------------------
      HandlerRequest (NotDidChangeTextDocument notification) -> do
        let doc :: LSP.Uri
            doc = notification ^. LSP.params . LSP.textDocument . LSP.uri
        mdoc <- liftIO $ LSP.Core.getVirtualFileFunc lf doc
        case mdoc of
          Just (VirtualFile _version str) -> do
            liftIO $ LSP.logs $
              "reactor:processing NotDidChangeTextDocument: vf got:" ++
              (show $ Yi.toString str)
          Nothing -> do
            liftIO $
              LSP.logs
                "reactor:processing NotDidChangeTextDocument: vf returned Nothing"
        liftIO $ LSP.logs $ "reactor:processing NotDidChangeTextDocument: uri=" ++
          (show doc)
      -- -------------------------------
      HandlerRequest (NotDidSaveTextDocument notification) -> do
        liftIO $ LSP.logm "****** reactor: processing NotDidSaveTextDocument"
        let doc = notification ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        liftIO $ LSP.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc Nothing
      -- -------------------------------
      HandlerRequest (ReqRename req) -> do
        liftIO $ LSP.logs $ "reactor:got RenameRequest:" ++ show req
        let _params = req ^. LSP.params
            _doc = _params ^. LSP.textDocument . LSP.uri
            LSP.Position _l _c' = _params ^. LSP.position
            _newName = _params ^. LSP.newName
        let we =
              LSP.WorkspaceEdit
                Nothing -- "changes" field is deprecated
                (Just (LSP.List [])) -- populate with actual changes from the rename
        let rspMsg = LSP.Core.makeResponseMessage req we
        reactorSend $ RspRename rspMsg
      -- -------------------------------
      HandlerRequest (ReqHover req) -> do
        liftIO $ LSP.logs $ "reactor:got HoverRequest:" ++ show req
        let LSP.TextDocumentPositionParams _doc pos = req ^. LSP.params
            LSP.Position _l _c' = pos
        let ht = LSP.Hover ms (Just range)
            ms =
              LSP.List [LSP.CodeString $ LSP.LanguageString "lsp-hello" "TYPE INFO"]
            range = LSP.Range pos pos
        reactorSend $ RspHover $ LSP.Core.makeResponseMessage req (Just ht)
      -- -------------------------------
      HandlerRequest (ReqCodeAction req) -> do
        liftIO $ LSP.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = req ^. LSP.params
            doc = params ^. LSP.textDocument
            -- fileName = drop (length ("file://"::String)) doc
            -- LSP.Range from to = LSP._range (params :: LSP.CodeActionParams)
            (LSP.List diags) = params ^. LSP.context . LSP.diagnostics
          -- makeCommand only generates commands for diagnostics whose source is us
        let makeCommand (LSP.Diagnostic (LSP.Range start _) _s _c (Just "lsp-hello") _m _l) =
              [LSP.Command title cmd cmdparams]
              where
                title = "Apply LSP hello command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
                cmd = "lsp-hello-command"
              -- need 'file' and 'start_pos'
                args =
                  LSP.List
                    [ Json.Object $
                      HashMap.fromList
                        [ ( "file"
                          , Json.Object $
                            HashMap.fromList [("textDocument", Json.toJSON doc)])
                        ]
                    , Json.Object $
                      HashMap.fromList
                        [ ( "start_pos"
                          , Json.Object $ HashMap.fromList [("position", Json.toJSON start)])
                        ]
                    ]
                cmdparams = Just args
            makeCommand (LSP.Diagnostic _r _s _c _source _m _l) = []
        let body = LSP.List $ map LSP.CACommand $ concatMap makeCommand diags
            rsp = LSP.Core.makeResponseMessage req body
        reactorSend $ RspCodeAction rsp
      -- -------------------------------
      HandlerRequest (ReqExecuteCommand req) -> do
        liftIO $ LSP.logs "reactor:got ExecuteCommandRequest:" -- ++ show req
        let params = req ^. LSP.params
            margs = params ^. LSP.arguments
        liftIO $ LSP.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
        let reply v =
              reactorSend $ RspExecuteCommand $ LSP.Core.makeResponseMessage req v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
            r = LSP.List [] :: LSP.List Int
        liftIO $ LSP.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            reply (Json.Object mempty)
            lid <- nextLspReqId
            -- reactorSend $ LSP.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            reactorSend $ ReqApplyWorkspaceEdit $
              fmServerApplyWorkspaceEditRequest lid we
          Nothing -> reply (Json.Object mempty)
      -- -------------------------------
      HandlerRequest om -> do
        liftIO $ LSP.logs $ "reactor:got HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------
toWorkspaceEdit :: t -> Maybe LSP.ApplyWorkspaceEditParams
toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------
-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: LSP.Uri -> Maybe Int -> R () ()
sendDiagnostics fileUri version = do
  let diags =
        [ LSP.Diagnostic
            (LSP.Range (LSP.Position 0 1) (LSP.Position 0 5))
            (Just LSP.DsWarning) -- severity
            Nothing -- code
            (Just "lsp-hello") -- source
            "Example diagnostic message"
            (Just (LSP.List []))
        ]
  -- reactorSend $ LSP.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just False
    , LSP._change = Just LSP.TdSyncFull
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.SaveOptions $ Just False
    }

lspOptions :: LSP.Core.Options
lspOptions = def {LSP.Core.textDocumentSync = Just syncOptions}

lspHandlers :: TChan ReactorInput -> LSP.Core.Handlers
lspHandlers rin =
  def {LSP.Core.initializedHandler = Just $ passHandler rin NotInitialized}

-- ---------------------------------------------------------------------
passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> LSP.Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

-- ---------------------------------------------------------------------
responseHandlerCb :: TChan ReactorInput -> LSP.Core.Handler LSP.BareResponseMessage
responseHandlerCb _rin resp = do
  LSP.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- Use Elm Compiler to compiler files
-- Arguments are root (where elm.json lives) and filenames (or [])
compileFiles root files = do
  liftIO $ Reporting.Task.try Reporting.Progress.Json.reporter $ do
    liftIO $ LSP.logs ("COMPILE1 " ++ root)
    project <- Elm.Project.Json.read (root </> "elm.json")
    liftIO $
      LSP.logs
        ("COMPILE2 " ++
         T.unpack (Elm.Package.toText (Elm.Project.Json.getName project)))
    Elm.Project.Json.check project
    liftIO $ LSP.logs ("COMPILE3 " ++ root)
    summary <- Stuff.Verify.verify root project
    -- If no files are given, get files from the  project
    actualFiles <-
      case files of
        Nothing -> liftIO $ getElmFiles project
        Just f -> return f
    liftIO $ LSP.logs ("COMPILE4 " ++ show actualFiles)
    args <- File.Args.fromPaths summary actualFiles
    liftIO $ LSP.logs ("COMPILE5 " ++ root)
    graph <- File.Crawl.crawl summary args
    liftIO $ LSP.logs ("COMPILE6 " ++ root)
    (dirty, ifaces) <- File.Plan.plan Nothing summary graph
    liftIO $ LSP.logs ("COMPILE7 " ++ root)
    File.Compile.compile project Nothing ifaces dirty

-- ---------------------------------------------------------------------
reportAnswers ::
     Maybe (Map.Map Elm.Compiler.Module.Raw File.Compile.Answer) -> R () ()
reportAnswers Nothing = return ()
reportAnswers (Just map) = do
  let list = Map.toList map
  sendDiagnostics
    (LSP.Uri $ T.pack $ Elm.Compiler.Module.nameToString (fst (head list)))
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
