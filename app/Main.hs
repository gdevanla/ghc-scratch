{-# LANGUAGE TupleSections #-}
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
import TcEnv
import IOEnv
import TcRnMonad
import DriverPhases (HscSource(HsSrcFile))
import SrcLoc
import GhcPlugins
import GhcMonad -- (Session, unGhc, withSession)
import FileCleanup
import GHC.IORef

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


sessionWithAct :: HscEnv -> Ghc a -> IO (HscEnv, a)
sessionWithAct env m = runGhc (Just libdir) $ do
  setSession env
  a <- m
  env <- getSession
  return (env, a)


-- eval :: String -> Ghc ()
-- eval inp = do
--   dyn <- fromDynamic <$> dynCompileExpr inp

eval :: String -> Ghc ()
eval inp = do
  dyn <-  fromDynamic <$> dynCompileExpr inp
  case dyn of
    Nothing -> do
      act <- compileExpr ("Prelude.print (" Prelude.<> inp Prelude.<> ")")
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

printNames fld (AConLike (RealDataCon dc))
  = Just [(showGhc dc, "")]
printNames _ (AnId id) = Just [(showGhc (Var.varName id), "")]
printNames _ x = Just [(showGhc x, "")]


fakeSpan :: RealSrcSpan
fakeSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<ghcide>") 1 1

lookupName :: GhcMonad m
           => Module -- ^ A module where the Names are in scope
           -> Name
           -> HscEnv
           -> m (Maybe (Maybe TyThing))
lookupName mod name env = withSession $ \env -> liftIO $ do
    (_messages, res) <- initTc env HsSrcFile False mod fakeSpan $ do
        tcthing <- tcLookup name
        case tcthing of
            AGlobal thing    -> return (Just thing)
            ATcId{tct_id=id} -> return (Just (AnId id))
            _ -> return Nothing
    return res


--runGhcEnv :: HscEnv -> Ghc a -> IO (HscEnv, a)

runGhcEnv env act = do
    filesToClean <- newIORef emptyFilesToClean
    dirsToClean <- newIORef mempty
    let dflags = (hsc_dflags env){filesToClean=filesToClean, dirsToClean=dirsToClean, useUnicode=True}
    ref <- newIORef env{hsc_dflags=dflags}
    res <- unGhc (act env) (Session ref) `finally` do
        cleanTempFiles dflags
        cleanTempDirs dflags
    (,res) <$> readIORef ref

main :: IO ()
main = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted }

  target <- guessTarget "Example.hs" Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName "Example"
  modSum' <- getModSummary $ mkModuleName "Example2"

  pmod <- parseModule modSum      -- ModuleSummary
  pmod' <- parseModule modSum'      -- ModuleSummary
  tmod <- typecheckModule pmod    -- TypecheckedSource
  tmod' <- typecheckModule pmod'
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = coreModule dmod      -- ModGuts
  --stg <- liftIO $ coreToStg dflags (mg_module core) (mg_binds core)


  liftIO $ putStrLn $ libdir

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc (parsedSource pmod)
  liftIO $ putStrLn $ showGhc (parsedSource pmod')
  liftIO $ putStrLn $ showGhc (parsedSource tmod')

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


  liftIO $ banner "From local modules"
  liftIO $ putStrLn $ showGhc $ findFields "FundCon1" (unLoc <$> hsmodDecls hsmodule)

  -- liftIO $ banner "Renamed Module"
  -- liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )
  liftIO $ banner "Other modules"
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
      getComplsForOne (GRE n _ True _) = return [] --("", "True")] --(showGhc n, show False)]
      getComplsForOne (GRE n _ False prov) = return [(showGhc n, (showGhc . is_mod . is_decl $ head prov))]
        -- case lookupTypeEnv typeEnv n of
        --   Just tt -> case nameMatches "FundConEx1" tt of
        --     Just result ->  return result
        --     Nothing -> return [(showGhc n, "not-found")]
        --   Nothing -> return [(showGhc n, "not-found-2")]
      -- getComplsForOne (GRE n _ False prov) =
      --   flip foldMapM (map is_decl prov) $ \spec -> do
      --     let parsedMod = tm_parsed_module tmod
      --         mod = (ms_mod $ pm_mod_summary parsedMod)
      --     --env <- initSession
      --     tyThing <- ghcCatch $ runGhcEnv env (Main.lookupName mod n)
      --     case tyThing of
      --       -- Just (_, Just (Just tt)) -> case nameMatches "FundEx1" tt of
      --       --     Just result -> return result
      --       --     Nothing -> liftIO $ (return [])
      --       -- _ -> liftIO $ return []
      --       Just (_, Just (Just tt)) -> case printNames "" tt of
      --         Just res -> return res
      --         Nothing -> return []
      --       _ -> return []

           --return $ name' >>= safeTyThingType
          -- compItem <- toCompItem curMod (is_mod spec) n
          -- let unqual
          --       | is_qual spec = []
          --       | otherwise = [compItem]
          --     qual
          --       | is_qual spec = Map.singleton asMod [compItem]
          --       | otherwise = Map.fromList [(asMod,[compItem]),(origMod,[compItem])]
          --     asMod = showModName (is_as spec)
          --     origMod = showModName (is_mod spec)
          -- return (unqual,QualCompls qual)

        -- toCompItem :: Module -> ModuleName -> Name -> IO CompItem
        -- toCompItem m mn n = do
        --   docs <- evalGhcEnv packageState $ getDocumentationTryGhc curMod (tm_parsed_module tm : deps) n
        --   ty <- evalGhcEnv packageState $ catchSrcErrors "completion" $ do
        --           name' <- lookupName m n
        --           return $ name' >>= safeTyThingType
        --   return $ mkNameCompItem n mn (either (const Nothing) id ty) Nothing docs




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
