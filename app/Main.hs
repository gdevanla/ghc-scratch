{-# LANGUAGE RecordWildCards #-}

module Main where

-- import GHC (
--   HscEnv, runGhc, getSessionDynFlags)
import GHC
import GHC.Paths
import GHC.LanguageExtensions
import DynFlags
import Outputable (Outputable, showPpr)

import Lib
import Data.Dynamic (fromDynamic)
import Control.Monad.IO.Class
import Unsafe.Coerce (unsafeCoerce)
import Control.Exception
import System.Console.Haskeline
import qualified Data.Text as T

banner :: MonadIO m => String -> m ()
banner msg =
  liftIO $
    putStrLn
      ( (replicate (fromIntegral n) '=')
          ++ msg
          ++ (replicate (fromIntegral n) '=')
      )
  where
    n = (76 - length msg) `div` 2


initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let dflags' = dflags {
                       hscTarget = HscInterpreted,
                       ghcLink = LinkInMemory
                       }
                `dopt_set` Opt_D_dump_BCOs
                `xopt_set` OverloadedStrings
  setSessionDynFlags dflags'
  setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]
  env <- getSession
  return env

session :: HscEnv -> Ghc a -> IO HscEnv
session env m = runGhc (Just libdir) $ do
  setSession env
  m
  env <- getSession
  return env


-- eval :: String -> Ghc ()
-- eval inp = do
--   dyn <- fromDynamic <$> dynCompileExpr inp

eval :: String -> Ghc ()
eval inp = do
  dyn <-  fromDynamic <$> dynCompileExpr inp
  case dyn of
    Nothing -> do
      act <- compileExpr ("Prelude.print (" <> inp <> ")")
      liftIO (unsafeCoerce act)
    Just act -> liftIO act

addImport :: String -> Ghc ()
addImport mod = do
  ctx <- getContext
  setContext ((IIDecl $ simpleImportDecl (mkModuleName mod)):ctx)

ghcCatch :: MonadIO m => IO a -> m (Maybe a)
ghcCatch m = liftIO $ do
  mres <- try m
  case mres of
    Left (SomeException err) -> do
      liftIO $ print err
      return Nothing
    Right res -> return (Just res)


repl:: HscEnv -> InputT IO ()
repl env = do
  minput <- getInputLine ">>>"
  case minput of
    Nothing -> outputStrLn "GoodBye"

    Just input | (T.pack "import") `T.isPrefixOf` (T.pack input) -> do
                   let mod = concat $ tail $ words input
                   env' <- ghcCatch $ session env $ addImport mod
                   maybe (repl env) repl env'

    Just input -> do
      env' <- ghcCatch (session env (eval input))
      maybe (repl env) repl env'

-- main :: IO ()
-- main = do
--   print "Strting session"
--   env <- initSession
--   runInputT defaultSettings (repl env)

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags


main :: IO ()
main = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted }

  target <- guessTarget "Example.hs" Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName "Example"

  pmod <- parseModule modSum      -- ModuleSummary
  tmod <- typecheckModule pmod    -- TypecheckedSource
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = coreModule dmod      -- ModGuts
  --stg <- liftIO $ coreToStg dflags (mg_module core) (mg_binds core)

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc (parsedSource pmod)

  let hsmodule = unLoc (parsedSource pmod)
  liftIO $ putStrLn $ showGhc hsmodule

  liftIO $ putStrLn $ showGhc (hsmodDecls hsmodule)

  let decl = unLoc $ head $ (hsmodDecls hsmodule)
  liftIO $ putStrLn $ showGhc decl

  case decl of
    TyClD _ (DataDecl{..}) -> do
      case tcdDataDefn of
        --HsDataDefn {..} -> liftIO $ putStrLn $ showGhc $ unLoc $ head dd_cons
        HsDataDefn {..} -> do
          case (unLoc. head $ dd_cons) of
            ConDeclGADT {..} -> liftIO $ putStrLn $ showGhc $ con_names
            ConDeclH98 {..} -> do
              case con_args of
                -- RecCon fld -> liftIO $ putStrLn $ showGhc $ unLoc <$> unLoc fld
                RecCon fld ->
                  case unLoc . head . unLoc $ fld of
                    ConDeclField {..} -> liftIO $ putStrLn $ showGhc $ unLoc cd_fld_type
                    otherwise -> liftIO $ putStrLn "XXConfField"
                otherwise -> liftIO $ putStrLn "not record"
        otherwise -> liftIO $ putStrLn "----"

      liftIO $ putStrLn $ showGhc $ tcdDataDefn
    _ -> liftIO $ putStrLn "nothing here"

  -- liftIO $ banner "Renamed Module"
  -- liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

  -- liftIO $ banner "Typechecked Module"
  -- liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

  -- liftIO $ banner "Typed Toplevel Definitions"
  -- liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

  -- liftIO $ banner "Typed Toplevel Exports"
  -- liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )

  --liftIO $ banner "Core Module"
  --liftIO $ putStrLn $ showGhc ( mg_binds core )

  --liftIO $ banner "STG"
  --liftIO $ putStrLn $ showGhc stg
