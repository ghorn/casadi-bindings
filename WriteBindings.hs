{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Control.Monad ( when )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List ( sort )
import Data.Maybe ( catMaybes )
import System.Directory ( doesFileExist )

--import WriteBindings.Buildbot.CasadiTree ( enums, tools, classes )
import WriteBindings.MyCasadiTree ( enums, tools, classes )
import WriteBindings.Buildbot.CasadiClasses
import qualified WriteBindings.Buildbot.CasadiTree as Buildbot
import qualified WriteBindings.WriteC as C
import qualified WriteBindings.WriteHs as HS
import WriteBindings.Types
import WriteBindings.WriteCTools

writeFile' :: FilePath -> String -> IO ()
writeFile' path txt = do
  exist <- doesFileExist path
  if exist
    then do txt0 <- fmap T.unpack $ TIO.readFile path
            when (txt0 /= txt) $ writeFile path txt
    else writeFile path txt

main :: IO ()
main = do
  let cOut = init $ unlines $
             [ "#include <swiginclude.hpp>"
             , "#include \"../marshal.hpp\""
             , "#include <symbolic/functor_internal.hpp>"
             , "#include <symbolic/fx/custom_function.hpp>"
             ] ++
             concatMap C.writeClass classes' ++
             map C.writeFunction tools'
      hsData = HS.writeDataModule classes' baseClasses
      hsClassModules = HS.writeClassModules baseClasses classes'
      hsToolsModule = HS.writeToolsModule tools'
      --hsIOSchemeHelpersModule = HS.writeIOSchemeHelpersModule ioschemeHelpers'
      hsEnumsModule = HS.writeEnumsModule enums

  writeFile' "Casadi/Wrappers/CToolsImports.hs" cToolsImports
  writeFile' "Casadi/Wrappers/CToolsInstances.hs" cToolsInstances

  writeFile' "cbits/autogen/all.cpp" cOut
  writeFile' "Casadi/Wrappers/Data.hs" hsData
  mapM_ (\(dataname, src) -> writeFile' ("Casadi/Wrappers/Classes/" ++ dataname ++ ".hs") src)  hsClassModules
  writeFile' "Casadi/Wrappers/Tools.hs" hsToolsModule
  --writeFile' "Casadi/Wrappers/IOSchemeHelpers.hs" hsIOSchemeHelpersModule
  writeFile' "Casadi/Wrappers/Enums.hs" hsEnumsModule
  writeFile' "Casadi/Wrappers/modules.txt" $
    unlines $ map ((\(dataname,_) -> "                       Casadi.Wrappers.Classes." ++ dataname)) hsClassModules

tools' :: [Function]
tools' = map addNamespace $ filter (not . functionHasBadTypes) tools
  where
    addNamespace :: Function -> Function
    addNamespace (Function (Name name) x y z) = Function (Name ("CasADi::"++name)) x y z

typeHasType :: Type -> Type -> Bool
typeHasType x y
  | x == y = True
typeHasType x (StdVec y) = typeHasType x y
typeHasType x (Ref y) = typeHasType x y
typeHasType x (ConstRef y) = typeHasType x y
typeHasType _ _ = False



-- all classes in Buildbot classes, but not in classes'
badTypes :: [Type]
badTypes = bc0 ++ (map Ref bc0) ++ (map ConstRef bc0)
  where
    bc0 = [StdOstream] ++ map CasadiClass (S.toList badClasses)

badClasses :: S.Set CasadiClass
badClasses = S.difference buildbotClasses goodClasses
  where
    goodClasses = S.fromList $ map (\(Class cc _ _) -> cc) (classes' ++ ioscheme)
    buildbotClasses = S.fromList $ map (\(Class cc _ _) -> cc) (Buildbot.classes ++ Buildbot.ioschemeclasses)

ioscheme :: [Class]
ioscheme = (filter (\(Class cc _ _) -> cc == IOScheme) Buildbot.ioschemeclasses)

classes' :: [Class]
classes' = map (addDocs . filterMethods) (classes ++ ioscheme)

addDocs :: Class -> Class
addDocs = id

filterMethods :: Class -> Class
filterMethods (Class ct methods docs) = Class ct methods' docs
  where
    methods' = filter (not . methodHasBadTypes) methods
    methodHasBadTypes :: Method -> Bool
    methodHasBadTypes method = any (flip methodHasType method) badTypes
      where
        methodHasType :: Type -> Method -> Bool
        methodHasType typ (Method _ ret params _ _) = any (typeHasType typ) (ret:params)

functionHasBadTypes :: Function -> Bool
functionHasBadTypes function = any (flip functionHasType function) badTypes
  where
    functionHasType :: Type -> Function -> Bool
    functionHasType typ (Function _ ret params _) = any (typeHasType typ) (ret:params)


baseClasses :: Class -> [Class]
baseClasses (Class classType _ _) = catMaybes $ map (lookup' classMap) (baseClasses' classType)
  where
    lookup' :: M.Map CasadiClass Class -> CasadiClass -> Maybe Class
    lookup' = flip M.lookup
--    lookup' :: M.Map CasadiClass Class -> CasadiClass -> Class
--    lookup' cm x = case M.lookup x cm of
--      Just y -> y
--      Nothing -> error $ "baseClasses lookup: can't find \"" ++ show x ++ "\" in:\n" ++ show (M.keys cm)

classMap :: M.Map CasadiClass Class
classMap = M.fromList $ map (\c@(Class cc _ _) -> (cc,c)) classes --  ++ ioschemeclasses)

baseClasses' :: CasadiClass -> [CasadiClass]
baseClasses' classType = case lookup classType inheritance of
  Nothing -> error $ "baseClasses': " ++ show classType ++ " missing from inheritance graph"
  Just xs -> unique $ xs ++ concatMap baseClasses' xs

unique :: Ord a => [a] -> [a]
unique = sort . S.toList . S.fromList
