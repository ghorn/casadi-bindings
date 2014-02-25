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
tools' = map addNamespace $ filter okTool tools
  where
    okTool :: Function -> Bool
    okTool x
      | hasType StdOstream x = False
      | hasType (Ref StdOstream) x = False
      | any (flip hasType x) badClasses' = False
      | otherwise = True

    addNamespace :: Function -> Function
    addNamespace (Function (Name name) x y z) = Function (Name ("CasADi::"++name)) x y z

-- all classes in Buildbot classes, but not in classes'
badClasses' :: [Type]
badClasses' = bc0 ++ (map Ref bc0) ++ (map ConstRef bc0)
  where
    bc0 = map CasadiClass (S.toList badClasses)

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
    methods' = filter (not . (hasTypes' [StdOstream, Ref StdOstream, ConstRef StdOstream])) methods

hasTypes' :: [Type] -> Method -> Bool
hasTypes' tps method = any (flip hasType' method) tps

hasType' :: Type -> Method -> Bool
hasType' typ (Method _ ret params _ _) = typ `elem` (ret:params)

hasType :: Type -> Function -> Bool
hasType typ (Function _ ret params _) = typ `elem` (ret:params)

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
