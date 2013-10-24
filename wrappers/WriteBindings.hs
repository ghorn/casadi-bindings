{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( sort )

import WriteCasadiBindings.Buildbot.CasadiTree
import WriteCasadiBindings.Buildbot.CasadiClasses
import qualified WriteCasadiBindings.WriteC as C
import qualified WriteCasadiBindings.WriteHs as HS
import WriteCasadiBindings.Types
import WriteCasadiBindings.WriteForeignTools

main :: IO ()
main = do
  let cOut = init $ unlines $
             [ "#include <swiginclude.hpp>"
             , "#include \"../marshal.hpp\""
             ] ++
             concatMap C.writeClass classes' ++
             map C.writeFunction tools' ++ map C.writeDeletes [CInt,CDouble,StdString,CBool]
      hsDeleters = HS.writeDeleterModule classes'
      hsData = HS.writeDataModule classes' baseClasses
      hsClassModules = HS.writeClassModules baseClasses classes'
      hsToolsModule = HS.writeToolsModule tools'
      hsIOSchemeHelpersModule = HS.writeIOSchemeHelpersModule ioschemeHelpers'
      hsEnumsModule = HS.writeEnumsModule enums

  writeFile "Casadi/Wrappers/ForeignToolsImports.hs" foreignToolsImports
  writeFile "Casadi/Wrappers/ForeignToolsInstances.hs" foreignToolsInstances

  writeFile "cbits/autogen/all.cpp" cOut
  writeFile "Casadi/Wrappers/Data.hs" hsData
  writeFile "Casadi/Wrappers/Deleters.hs" hsDeleters
  mapM_ (\(dataname, src) -> writeFile ("Casadi/Wrappers/Classes/" ++ dataname ++ ".hs") src)  hsClassModules
  writeFile "Casadi/Wrappers/Tools.hs" hsToolsModule
  writeFile "Casadi/Wrappers/IOSchemeHelpers.hs" hsIOSchemeHelpersModule
  writeFile "Casadi/Wrappers/Enums.hs" hsEnumsModule
  writeFile "Casadi/Wrappers/modules.txt" $
    unlines $ map ((\(dataname,_) -> "                       Casadi.Wrappers.Classes." ++ dataname)) hsClassModules

tools' :: [Function]
tools' = map addNamespace $ filter (not . hasStdOstream) tools
  where
    addNamespace :: Function -> Function
    addNamespace (Function (Name name) x y z) = Function (Name ("CasADi::"++name)) x y z

ioschemeHelpers' :: [Function]
ioschemeHelpers' = map addNamespace $ filter (not . hasStdOstream) ioschemehelpers
  where
    addNamespace :: Function -> Function
    addNamespace (Function (Name name) x y z) = Function (Name ("CasADi::"++name)) x y z


classes' :: [Class]
classes' = map filterStdOstreams (classes ++ ioschemeclasses)

filterStdOstreams :: Class -> Class
filterStdOstreams (Class cc methods docs) = Class cc methods' docs
  where
    methods' = filter (not . hasStdOstream') methods

-- remove methods with StdOStrea'
hasStdOstream' :: Method -> Bool
hasStdOstream' (Method _ ret params _ _) = StdOstream `elem` (map getPrim (ret:params))

-- remove methods with StdOStrea'
hasStdOstream :: Function -> Bool
hasStdOstream (Function _ _ params _) = StdOstream `elem` (map getPrim params)

getPrim :: Type -> Primitive
getPrim (Val x) = getPrimTV x
getPrim (Ref x) = getPrimTV x
getPrim (ConstRef x) = getPrimTV x

getPrimTV :: ThreeVectors -> Primitive
getPrimTV (NonVec x) = x
getPrimTV (Vec (NonVec x)) = x
getPrimTV (Vec (Vec (NonVec x))) = x
getPrimTV (Vec (Vec (Vec (NonVec x)))) = x
getPrimTV (Vec (Vec (Vec (Vec ())))) = error "getPrimTV: Vec (Vec (Vec (Vec ())))"


baseClasses :: Class -> [Class]
baseClasses (Class classType _ _) = map (lookup' classMap) (baseClasses' classType)
  where
    lookup' cm x = case M.lookup x cm of
      Just y -> y
      Nothing -> error $ "baseClasses lookup: can't find \"" ++ show x ++ "\" in:\n" ++ show (M.keys cm)

classMap :: M.Map CasadiClass Class
classMap = M.fromList $ map (\c@(Class cc _ _) -> (cc,c)) (classes ++ ioschemeclasses)

baseClasses' :: CasadiClass -> [CasadiClass]
baseClasses' classType = case lookup classType inheritance of
  Nothing -> error $ "baseClasses': " ++ show classType ++ " missing from inheritance graph"
  Just xs -> unique $ xs ++ concatMap baseClasses' xs

unique :: Ord a => [a] -> [a]
unique = sort . S.toList . S.fromList
