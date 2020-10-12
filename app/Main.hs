{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- import GHC (
--   HscEnv, runGhc, getSessionDynFlags)
import GHC
import GHC.Hs
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
import Data.Maybe
import RdrName
import OccName (occNameString)
import TcRnTypes
import Id
import qualified Var
import ConLike
import DataCon
import HscTypes (lookupTypeEnv)

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


-- findFields :: String -> [LHsDecl a] -> String
-- findFields ctxStr decls = let
--   unLocs = unLoc <$> decls
--   filtDecl = findDecl unLocs

--   findDecl :: [HsDecl a] -> HsDecl a
--   findDecl = filter isMatch
--     where
--       isMatch decl =
--         case decl of
--           TyClD _ (DataDecl{..}) -> do
--             case tcdDataDefn of
--               HsDataDefn {..} -> do
--                 let conDecls = filter isMatch' $ unLoc <$> dd_cons
--                       where
--                     isMatch' conDecl = case condDecl of
--                       ConDeclGADT {..} -> False -- TODO: Support GADTs
--                       ConDeclH98 {..} -> do
--                   case con_args of
--                     -- RecCon fld -> liftIO $ putStrLn $ showGhc $ unLoc <$> unLoc fld
--                     RecCon fld ->
--                       case unLoc . head . unLoc $ fld of
--                         ConDeclField {..} -> liftIO $ putStrLn $ showGhc $ unLoc cd_fld_type
--                         otherwise -> liftIO $ putStrLn "XXConfField"
--                 otherwise -> liftIO $ putStrLn "not record"
--         otherwise -> liftIO $ putStrLn "----"


-- findFields :: String -> [LHsDecl a] -> String
-- findFields  ctxStr decls =
--   let
--     tyclDecls = [decl | decl <- listify (\(_ :: (TyCld GhcPs)) -> True) decls]
--     dataDefns = [tcdDataDefn decl | decl <- listify (\(_ :: (DataDecl GhcPs)) -> True) tyclDecls]
--     --conDeclH98 = [decl | decl <- listify (\(_ :: ConDeclH98)$ (unLoc <$> (dd_cons dataDefns))]
--   in
--     dataDefns

--findFields :: String -> [LHsDecl a] -> String
findFields :: String -> [HsDecl GhcPs] -> [(Located RdrName, HsType GhcPs)]
findFields  ctxStr decls = name_type
  where
    dataDefns = catMaybes $ findDataDefns <$> decls
    findDataDefns decl =
      case decl of
        TyClD _ (DataDecl{tcdDataDefn}) -> Just tcdDataDefn
        _ -> Nothing
    conDecls = concat [ unLoc <$> dd_cons dataDefn | dataDefn <- dataDefns]
    h98 = catMaybes $ findH98 <$> conDecls
    findH98 conDecl = case conDecl of
      ConDeclH98{..} -> Just (unLoc con_name, con_args)
      ConDeclGADT{..} -> Nothing  -- TODO: Expand this out later
    conArgs = [snd x | x  <- h98, (occNameString . rdrNameOcc . fst $ x) == ctxStr]
    flds = concat . catMaybes $ getFlds <$> conArgs
    getFlds conArg = case conArg of
      RecCon rec -> Just $ unLoc <$> (unLoc rec)
      otherwise -> Nothing
    name_type = extract <$> flds
    extract ConDeclField{..} = let
      fld_type = unLoc cd_fld_type
      fld_name = rdrNameFieldOcc $ unLoc . head $ cd_fld_names --TODO: Why is cd_fld_names is a list?
        in
        (fld_name, fld_type)


-- safeTyThingId :: TyThing -> True
-- safeTyThingId (AConLike (RealDataCon dc)) = dataConWrapId dc
-- safeTyThingId _ = False

--nameMatches :: String -> TyThing -> Maybe [()]

nameMatches fld (AConLike (RealDataCon dc))
  | showGhc (Var.varName (dataConWrapId dc)) == fld =
      Just [(showGhc $ flLabel lbl, showGhc $ dataConFieldType dc (flLabel lbl))
           | lbl <- dataConFieldLabels dc]
  | otherwise = Nothing
nameMatches _  _ = Nothing

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


  liftIO $ putStrLn $ showGhc $ findFields "FundCon1" (unLoc <$> hsmodDecls hsmodule)

  -- liftIO $ banner "Renamed Module"
  -- liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

  let foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
      foldMapM f xs = foldr step return xs mempty where
        step x r z = f x >>= \y -> r $! (z `mappend` y)

      -- getCompl :: Int -> IO [Int]
      -- getCompl x = return [(x * 2)]

  -- compls <- liftIO $ foldMapM getCompl [1, 2, 3]
  -- liftIO $ banner "Typechecked Module"
  --liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

  -- liftIO $ putStrLn $ show compls

  let typeEnv = tcg_type_env $ fst $ tm_internals_ tmod
      rdrEnv = tcg_rdr_env $ fst $ tm_internals_ tmod
      rdrElts = globalRdrEnvElts rdrEnv

      compl = getCompls rdrElts

      getCompls :: [GlobalRdrElt] -> IO [(String, String)]
      getCompls = foldMapM getComplsForOne

      --getComplsForOne :: GlobalRdrElt -> IO ([String, String])
      getComplsForOne :: GlobalRdrElt -> IO [(String, String)]
      getComplsForOne (GRE n _ True _) =
        case lookupTypeEnv typeEnv n of
          Just tt -> case nameMatches "FundCon1" tt of
            Just result ->  return result
            Nothing -> return []
          Nothing -> return []
      getComplsForOne (GRE n _ False prov) = return []

      -- --varToTuple :: Var -> IO ()
      -- varToTupl :: Var -> IO [(String, String)]
      -- varToTupl var = do
      --   let typ = Just $ Var.varType var
      --       name = Var.varName var
      --   return $ [(showGhc name, showGhc typ)]

  compls <- liftIO $ compl
  liftIO $ putStrLn $ show $ compls

  liftIO $ putStrLn "Done"





      --getCompls :: [GlobalRdrElt] -> IO ([CompItem],QualCompls)
      --getCompls = foldMapM getComplsForOne

      -- getComplsForOne :: GlobalRdrElt -> IO ([CompItem],QualCompls)
      -- getComplsForOne (GRE n _ True _) =
      --   case lookupTypeEnv typeEnv n of
      --     Just tt -> case safeTyThingId tt of
      --       Just var -> (\x -> ([x],mempty)) <$> varToCompl var
      --       Nothing -> (\x -> ([],mempty)) <$> [] -- toCompItem curMod curModName n
      --     Nothing -> (\x -> ([],mempty)) <$> [] -- toCompItem curMod curModName n
      -- getComplsForOne (GRE n _ False prov) =
      --   flip foldMapM (map is_decl prov) $ \spec -> do
      --     compItem <- toCompItem curMod (is_mod spec) n
      --     let unqual
      --           | is_qual spec = []
      --           | otherwise = [compItem]
      --         qual
      --           | is_qual spec = Map.singleton asMod [compItem]
      --           | otherwise = Map.fromList [(asMod,[compItem]),(origMod,[compItem])]
      --         asMod = showModName (is_as spec)
      --         origMod = showModName (is_mod spec)
      --     return (unqual,QualCompls qual)

      -- --varToCompl :: Var -> IO ()
      -- varToCompl :: Var -> (Name, Maybe Kind)
      -- varToCompl var = do
      --   let typ = Just $ varType var
      --       name = Var.varName var
      --       return $ (name, typ)

      -- toCompItem :: Module -> ModuleName -> Name -> IO CompItem
      -- toCompItem m mn n = do
      --   docs <- evalGhcEnv packageState $ getDocumentationTryGhc curMod (tm_parsed_module tm : deps) n
      --   ty <- evalGhcEnv packageState $ catchSrcErrors "completion" $ do
      --           name' <- lookupName m n
      --           return $ name' >>= safeTyThingType
      --   return $ mkNameCompItem n mn (either (const Nothing) id ty) Nothing docs


  -- liftIO $ banner "Typed Toplevel Definitions"
  -- liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

  -- liftIO $ banner "Typed Toplevel Exports"
  -- liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )

  --liftIO $ banner "Core Module"
  --liftIO $ putStrLn $ showGhc ( mg_binds core )

  --liftIO $ banner "STG"
  --liftIO $ putStrLn $ showGhc stg
