{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Control.Monad ( when )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( sort )
import System.Directory ( doesFileExist )
import System.IO ( openFile, hClose, IOMode(..), hGetContents )

import WriteCasadiBindings.Buildbot.CasadiTree
import WriteCasadiBindings.Buildbot.CasadiClasses
import qualified WriteCasadiBindings.WriteC as C
import qualified WriteCasadiBindings.WriteHs as HS
import WriteCasadiBindings.Types
import WriteCasadiBindings.WriteForeignTools

writeFile' :: FilePath -> String -> IO ()
writeFile' path txt = do
  exist <- doesFileExist path
  if exist
    then do fileHandle <- openFile path ReadMode
            txt0 <- hGetContents fileHandle
            hClose fileHandle
            when (txt0 /= txt) $ writeFile path txt
    else writeFile path txt

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

  writeFile' "Casadi/Wrappers/ForeignToolsImports.hs" foreignToolsImports
  writeFile' "Casadi/Wrappers/ForeignToolsInstances.hs" foreignToolsInstances

  writeFile' "cbits/autogen/all.cpp" cOut
  writeFile' "Casadi/Wrappers/Data.hs" hsData
  writeFile' "Casadi/Wrappers/Deleters.hs" hsDeleters
  mapM_ (\(dataname, src) -> writeFile' ("Casadi/Wrappers/Classes/" ++ dataname ++ ".hs") src)  hsClassModules
  writeFile' "Casadi/Wrappers/Tools.hs" hsToolsModule
  writeFile' "Casadi/Wrappers/IOSchemeHelpers.hs" hsIOSchemeHelpersModule
  writeFile' "Casadi/Wrappers/Enums.hs" hsEnumsModule
  writeFile' "Casadi/Wrappers/modules.txt" $
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
classes' = map (addGenerics . filterStdOstreams) (classes ++ ioschemeclasses)

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

--    GenericType(bool b);
--    GenericType(int i);
--    GenericType(double d);
--    GenericType(const std::string& s);
--    GenericType(const std::vector<bool>& iv);
--    GenericType(const std::vector<int>& iv);
--    GenericType(const std::vector<double>& dv);
--    GenericType(const std::vector<std::string>& sv);
--    GenericType(const char s[]);
--    GenericType(const FX& f);

addGenerics :: Class -> Class
addGenerics (Class GenericType methods docs) = Class GenericType (methods ++ moreGenerics) docs
addGenerics x = x

moreGenerics :: [Method]
moreGenerics =
  [ Method (Name "GenericTypeBool") valGenericType [valCBool] Constructor (Doc "")
  , Method (Name "GenericTypeInt") valGenericType [valCInt] Constructor (Doc "")
  , Method (Name "GenericTypeDouble") valGenericType [valCDouble] Constructor (Doc "")
  , Method (Name "GenericTypeString") valGenericType [constrefStdString] Constructor (Doc "")
  , Method (Name "GenericTypeBoolVec") valGenericType [refCBoolVec] Constructor (Doc "")
  , Method (Name "GenericTypeIntVec") valGenericType [constrefCIntVec] Constructor (Doc "")
  , Method (Name "GenericTypeDoubleVec") valGenericType [constrefCDoubleVec] Constructor (Doc "")
  , Method (Name "GenericTypeStringVec") valGenericType [constrefStdStringVec] Constructor (Doc "")
  , Method (Name "GenericTypeFX") valGenericType [constrefFX] Constructor (Doc "")
  ]

valGenericType :: Type
valGenericType = Val (NonVec (CasadiClass GenericType))

valCBool :: Type
valCBool = Val (NonVec CBool)
valCInt :: Type
valCInt = Val (NonVec CInt)
valCDouble :: Type
valCDouble = Val (NonVec CDouble)
constrefStdString :: Type
constrefStdString = ConstRef (NonVec StdString)
refCBoolVec :: Type
refCBoolVec = Ref (Vec (NonVec CBool))
constrefCIntVec :: Type
constrefCIntVec = ConstRef (Vec (NonVec CInt))
constrefCDoubleVec :: Type
constrefCDoubleVec = ConstRef (Vec (NonVec CDouble))
constrefStdStringVec :: Type
constrefStdStringVec = ConstRef (Vec (NonVec StdString))
constrefFX :: Type
constrefFX = ConstRef (NonVec (CasadiClass FX))
