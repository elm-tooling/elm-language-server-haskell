{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Language.Elm where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad.IO.Class
import Control.Monad
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- ELM COMPILER MODULES
import qualified Compile
import qualified Parse.Parse
import qualified Canonicalize.Module
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
import qualified Reporting.Task
import qualified Reporting.Doc
import qualified Reporting.Error
import qualified Reporting.Render.Code
import qualified Reporting.Report
import qualified Reporting.Progress
import qualified Reporting.Task
import qualified Reporting.Warning
import qualified Reporting.Result
import qualified Reporting.Render.Type.Localizer
import qualified "elm" Reporting.Region -- would conflict with elm-format's Reporting.Region
import qualified Stuff.Verify

instance Show Elm.Name.Name where
    show = Elm.Name.toString

instance Show Elm.Compiler.Module.Canonical where
    show (Elm.Compiler.Module.Canonical pkgName moduleName) = show pkgName ++ ":" ++ show moduleName

instance Show Elm.Package.Name where
    show (Elm.Package.Name author name) = T.unpack author ++ "/" ++ T.unpack name

data TypeCheckSuccess = TypeCheckSuccess
    { interface :: Elm.Interface.Interface
    }

data TypeCheckFailure = TypeCheckFailure
    { sourcePath :: FilePath
    , rawSource :: BS.ByteString
    , errors :: [Elm.Compiler.Error]
    }

type CheckingAnswer = Either TypeCheckFailure TypeCheckSuccess
type CheckingAnswers = Map.Map Elm.Compiler.Module.Raw CheckingAnswer

-- Use Elm Compiler to typecheck files
-- Arguments are root (where elm.json lives) and filenames (or [])
typeCheckFiles :: MonadIO m => String -> m (Maybe CheckingAnswers)
typeCheckFiles root = do
    terminalReporter <- liftIO Reporting.Progress.Terminal.create
    liftIO $ Reporting.Task.try terminalReporter $ do
        project <- Elm.Project.Json.read (root </> "elm.json")
        Elm.Project.Json.check project
        summary <- Stuff.Verify.verify root project
        -- get files from the  project
        files <- liftIO $ getElmFiles project

        args <- File.Args.fromPaths summary files
        graph <- File.Crawl.crawl summary args
        (dirtyModules, interfaces) <- File.Plan.plan Nothing summary graph

        answers <- liftIO $ do
            mvar <- newEmptyMVar
            iMVar <- newMVar interfaces
            answerMVars <- Map.traverseWithKey (compileModule project mvar iMVar) dirtyModules
            putMVar mvar answerMVars
            traverse readMVar answerMVars

        return (fmap computeInterfaceOrErrors answers)

computeInterfaceOrErrors :: File.Compile.Answer -> CheckingAnswer
computeInterfaceOrErrors answer =
    case answer of
        File.Compile.Blocked ->
            Left (TypeCheckFailure "" BS.empty [])

        File.Compile.Bad path timeStamp source errors ->
            Left (TypeCheckFailure path source errors)

        File.Compile.Good (Elm.Compiler.Artifacts interface output documentation) ->
            Right (TypeCheckSuccess interface)

compileModule
    :: Elm.Project.Json.Project
    -> MVar (Map.Map Elm.Compiler.Module.Raw (MVar File.Compile.Answer))
    -> MVar Elm.Interface.Interfaces
    -> Elm.Compiler.Module.Raw
    -> File.Plan.Info
    -> IO (MVar File.Compile.Answer)
compileModule project answersMVar ifacesMVar name info = do
    mvar <- newEmptyMVar
    void $ forkIO $
        do  answers <- readMVar answersMVar
            blocked <- File.Compile.isBlocked answers info
            if blocked
            then putMVar mvar File.Compile.Blocked
            else
                do  let pkg = Elm.Project.Json.getName project
                    let imports = File.Compile.makeImports project info
                    ifaces <- readMVar ifacesMVar
                    let source = File.Plan._src info
                    case customCompile pkg imports ifaces source of
                        (_warnings, Left errors) -> do
                            let time = File.Plan._time info
                            let path = File.Plan._path info
                            putMVar mvar (File.Compile.Bad path time source errors)

                        (_warnings, Right result@(Compile.Artifacts elmi _ _)) -> do
                            let canonicalName = Elm.Compiler.Module.Canonical pkg name
                            lock <- takeMVar ifacesMVar
                            putMVar ifacesMVar (Map.insert canonicalName elmi lock)
                            putMVar mvar (File.Compile.Good result)

    return mvar

customCompile
    :: Elm.Package.Name
    -> Map.Map Elm.Compiler.Module.Raw Elm.Compiler.Module.Canonical
    -> Map.Map Elm.Compiler.Module.Canonical Elm.Compiler.Module.Interface
    -> BS.ByteString
    -> ([Reporting.Warning.Warning], Either [Reporting.Error.Error] Elm.Compiler.Artifacts)
customCompile pkg importDict interfaces source =
    Reporting.Result.run $ do
        valid <- Reporting.Result.mapError Reporting.Error.Syntax $
            Parse.Parse.program pkg source

        canonical <- Reporting.Result.mapError Reporting.Error.Canonicalize $
            Canonicalize.Module.canonicalize pkg importDict interfaces valid

        let localizer = Reporting.Render.Type.Localizer.fromModule valid

        annotations <-
            Compile.runTypeInference localizer canonical

        () <-
            Compile.exhaustivenessCheck canonical

        Reporting.Result.ok $
            Compile.Artifacts
                { Compile._elmi = Elm.Interface.fromModule annotations canonical
                , Compile._elmo = AST.Optimized.Graph Map.empty Map.empty Map.empty
                , Compile._docs = Nothing
                }


-- Get all elm files given in an elm.json ([] for a package, all elm files for an application)
getElmFiles :: Elm.Project.Json.Project -> IO [FilePath]
getElmFiles summary = case summary of
    Elm.Project.Json.App app -> do
        let dirs = Elm.Project.Json._app_source_dirs app
        elmFiles <- mapM (Glob.globDir1 (Glob.compile "**/*.elm")) dirs
        return (concat elmFiles)
    Elm.Project.Json.Pkg package -> return []
