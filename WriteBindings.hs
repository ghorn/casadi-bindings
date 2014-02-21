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

--import qualified WriteCasadiBindings.Buildbot.CasadiTree as Buildbot
import WriteCasadiBindings.MyCasadiTree ( enums, tools, classes )
import WriteCasadiBindings.Buildbot.CasadiClasses
import qualified WriteCasadiBindings.Buildbot.CasadiTree as Buildbot
import qualified WriteCasadiBindings.WriteC as C
import qualified WriteCasadiBindings.WriteHs as HS
import WriteCasadiBindings.Types
import WriteCasadiBindings.WriteForeignTools

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
             map C.writeFunction tools' ++ map C.writeDeletes [CInt,CDouble,StdString,CBool]
      hsDeleters = HS.writeDeleterModule classes'
      hsData = HS.writeDataModule classes' baseClasses
      hsClassModules = HS.writeClassModules baseClasses classes'
      hsToolsModule = HS.writeToolsModule tools'
      --hsIOSchemeHelpersModule = HS.writeIOSchemeHelpersModule ioschemeHelpers'
      hsEnumsModule = HS.writeEnumsModule enums

  writeFile' "Casadi/Wrappers/ForeignToolsImports.hs" foreignToolsImports
  writeFile' "Casadi/Wrappers/ForeignToolsInstances.hs" foreignToolsInstances

  writeFile' "cbits/autogen/all.cpp" cOut
  writeFile' "Casadi/Wrappers/Data.hs" hsData
  writeFile' "Casadi/Wrappers/Deleters.hs" hsDeleters
  mapM_ (\(dataname, src) -> writeFile' ("Casadi/Wrappers/Classes/" ++ dataname ++ ".hs") src)  hsClassModules
  writeFile' "Casadi/Wrappers/Tools.hs" hsToolsModule
  --writeFile' "Casadi/Wrappers/IOSchemeHelpers.hs" hsIOSchemeHelpersModule
  writeFile' "Casadi/Wrappers/Enums.hs" hsEnumsModule
  writeFile' "Casadi/Wrappers/modules.txt" $
    unlines $ map ((\(dataname,_) -> "                       Casadi.Wrappers.Classes." ++ dataname)) hsClassModules

tools' :: [Function]
tools' = map addNamespace $ filter okTool tools
  where
    okTool :: Function -> Bool
    okTool x
      | hasType StdOstream x = False
      | hasType StdOstream x = False
      | any (flip hasType x) badClasses' = False
      | otherwise = True

    addNamespace :: Function -> Function
    addNamespace (Function (Name name) x y z) = Function (Name ("CasADi::"++name)) x y z

-- all classes in Buildbot classes, but not in classes'
badClasses' :: [Primitive]
badClasses' = map CasadiClass (S.toList badClasses)

badClasses :: S.Set CasadiClass
badClasses = S.difference buildbotClasses goodClasses
  where
    goodClasses = S.fromList $ map (\(Class cc _ _) -> cc) classes'
    buildbotClasses = S.fromList $ map (\(Class cc _ _) -> cc) Buildbot.classes

classes' :: [Class]
classes' = map (addDocs . filterStdOstreams) classes -- ++ ioschemeclasses)

addDocs :: Class -> Class
addDocs = id

filterStdOstreams :: Class -> Class
filterStdOstreams (Class cc methods docs) = Class cc methods' docs
  where
    methods' = filter (not . (hasType' StdOstream)) methods

hasType' :: Primitive -> Method -> Bool
hasType' typ (Method _ ret params _ _) = typ `elem` (map getPrim (ret:params))

hasType :: Primitive -> Function -> Bool
hasType typ (Function _ ret params _) = typ `elem` (map getPrim (ret:params))

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
