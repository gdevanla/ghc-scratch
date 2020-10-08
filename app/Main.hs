module Main where

-- import GHC (
--   HscEnv, runGhc, getSessionDynFlags)
import GHC
import GHC.Paths
import GHC.LanguageExtensions
import DynFlags

import Lib
import Data.Dynamic (fromDynamic)
import Control.Monad.IO.Class
import Unsafe.Coerce (unsafeCoerce)
import Control.Exception
import System.Console.Haskeline
import qualified Data.Text as T


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
                   env' <- ghcCatch (session env (eval input))
                   maybe (repl env) repl env'

    Just input -> do
      env' <- ghcCatch (session env (eval input))
      maybe (repl env) repl env'


main :: IO ()
main = do
  print "Strting session"
  env <- initSession
  runInputT defaultSettings (repl env)
