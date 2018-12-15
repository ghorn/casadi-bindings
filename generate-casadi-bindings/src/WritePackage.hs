{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Control.Monad ( when )
import Control.Arrow ( first )
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Char ( toUpper )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory ( doesFileExist, createDirectoryIfMissing )
import System.FilePath.Posix

import qualified WriteBindings.WriteC as C
import qualified WriteBindings.WriteHs as HS
import WriteBindings.ParseJSON
import System.Process ( CreateProcess(..), createProcess, shell, waitForProcess )

version :: String
version = "3.4.5.0"

internalVersion :: String
internalVersion = "0.1.6.1"

jsonpath :: FilePath
jsonpath = "/home/greg/casadi/build/swig/json/casadi.json"

writepath :: FilePath
writepath = "/home/greg/hslibs/casadi-bindings/genpath"

writeFile' :: FilePath -> String -> IO ()
writeFile' path txt = do
  createDirectoryIfMissing True (takeDirectory path)
  exist <- doesFileExist path
  if exist
    then do txt0 <- fmap T.unpack $ TIO.readFile path
            when (txt0 /= txt) $ do putStrLn $ "   (changed) " ++ path
                                    writeFile path txt
    else do putStrLn $ "   (new)     " ++ path
            writeFile path txt

data Package =
  Package
  { pCabal :: (FilePath, String)
  , pStack :: (FilePath, String)
  , pCClasses :: (FilePath,String)
  , pCFunctions :: (FilePath,String)
  , pHsClasses :: [(FilePath,String)]
  , pHsTools :: Maybe (FilePath,String)
  , pHsEnums :: Maybe (FilePath,String)
  , pHsData :: (FilePath,String)
  , pModuleName :: String
  }

writePackage :: FilePath -> Package -> IO ()
writePackage rootDir pkg = do
  putStrLn $ "writing " ++ (pModuleName pkg)
  let wf :: (FilePath, String) -> IO ()
      wf (fp,contents) = writeFile' (rootDir </> pModuleName pkg </> fp) contents

  license <- readFile (rootDir </> ".." </> "casadi-bindings-internal" </> "LICENSE")
  setupFile <- readFile (rootDir </> ".." </> "casadi-bindings-internal" </> "Setup.hs")
  marshallHpp <- readFile (rootDir </> ".." </> "generate-casadi-bindings" </> "src" </> "WriteBindings" </> "marshal.hpp")
  customWrappersCpp <- readFile (rootDir </> ".." </> "generate-casadi-bindings" </> "src" </> "WriteBindings" </> "custom_wrappers.cpp")
  customWrappersHs <- readFile (rootDir </> ".." </> "generate-casadi-bindings" </> "src" </> "WriteBindings" </> "CustomWrappers.hs")

  wf ("LICENSE",license)
  wf ("cbits" </> "marshal.hpp",marshallHpp)
  wf ("cbits" </> "custom_wrappers.cpp",customWrappersCpp)
  wf ("Casadi" </> "Core" </> "CustomWrappers.hs",customWrappersHs)
  wf ("Setup.hs",setupFile)
  wf (pCabal pkg)
  wf (pStack pkg)
  wf (pCClasses pkg)
  wf (pCFunctions pkg)
  wf (pHsData pkg)
  mapM_ wf (pHsClasses pkg)
  F.traverse_ wf (pHsTools pkg)
  F.traverse_ wf (pHsEnums pkg)

uppercase :: String -> String
uppercase (x:xs) = toUpper x : xs
uppercase [] = error "uppercase got empty string"

toPackage :: Module -> Package
toPackage mod' =
  Package
  { pCClasses = ("cbits/autogen/casadi_wrap_classes.cpp",
                 unlines $ cincludes ++ [""] ++
                 map C.writeClass (M.elems (moduleClasses mod'))
                )
  , pCFunctions = ("cbits/autogen/casadi_wrap_functions.cpp",
                 unlines $ cincludes ++ [""] ++
                 concatMap C.writeFunctions (moduleFunctions mod')
                )
  , pHsTools = case moduleFunctions mod' of
       [] -> Nothing
       xs -> Just ("Casadi/Core/Tools.hs", HS.writeToolsModule xs)
  , pHsClasses = map (first (\n -> "Casadi/Core/Classes/" ++ n ++ ".hs")) hsClassModules
  , pHsEnums = case (M.toList (moduleEnums mod')) of
    --[] -> Nothing
    xs -> Just ("Casadi/Core/Enums.hs", HS.writeEnumsModule xs)
  , pModuleName = modname
  , pHsData = ("Casadi/Core/Data.hs",
               HS.writeDataModule allclasses inherit)
  , pStack = ( "stack.yaml"
             , unlines
               [ "resolver: lts-12.21"
               , ""
               , "compiler-check: newer-minor"
               , ""
               , "# Local packages, usually specified by relative directory name"
               , "packages:"
               , "- ."
               , ""
--               , "- location:"
--               , "    ../../casadi-bindings-internal"
--               , ""
               , "# Packages to be pulled from upstream that are not in the resolver (e.g., acme-missiles-0.3)"
               , "extra-deps: [ casadi-bindings-internal-" ++ internalVersion
               , "            ]"
               ]
             )
  , pCabal = (modname ++ ".cabal",
              unlines $
              [ "name:                " ++ modname
              , "version:             " ++ version
              , "license:             LGPL-3"
              , "license-file:        LICENSE"
              , "category:            Numerical, Math"
              , "copyright:           (c) 2013-2018 Greg Horn"
              , "author:              Greg Horn"
              , "maintainer:          gregmainland@gmail.com"
              , "build-type:          Simple"
              , "cabal-version:       >=1.10"
              , "synopsis:            autogenerated low level bindings to casadi"
              , "description:         see <http://hackage.haskell.org/package/casadi-bindings http://hackage.haskell.org/package/casadi-bindings> for all instructions"
              , "extra-source-files:  cbits/marshal.hpp"
              , ""
              , "library"
              , "  build-depends:       base >=4.6 && <5,"
              , "                       vector >=0.10,"
              , "                       containers >= 0.5,"
              , "                       casadi-bindings-internal == " ++ internalVersion
              , ""
              , "  default-language:    Haskell2010"
              , ""
              , ""
              , "  extra-libraries:  stdc++ casadi"
              , ""
              , "  ghc-prof-options: -O2"
              , "  ghc-options:      -O2"
              , "  cc-options: -Wall -Wno-sign-compare -std=c++11"
              , ""
              , "  C-sources:        cbits/custom_wrappers.cpp"
              , "                    cbits/autogen/casadi_wrap_classes.cpp"
              , "                    cbits/autogen/casadi_wrap_functions.cpp"
              , ""
              , "  exposed-modules:  Casadi.Core.Data"
              , "                    Casadi.Core.Enums"
              , "                    Casadi.Core.Tools"
              , "                    Casadi.Core.CustomWrappers"
              ] ++ map (\(n,_) -> "                    " ++
                                "Casadi.Core.Classes." ++ uppercase n) hsClassModules
              )
  }
  where
    modname = "casadi-bindings-core"

    inherit :: ClassType -> S.Set ClassType
    inherit = baseClasses (moduleInheritance mod')

    cincludes :: [String]
    cincludes =
      moduleIncludes mod' ++
      [ "#include \"../marshal.hpp\""
      ]

    allclasses = M.elems (moduleClasses mod')

    hsClassModules :: [(String,String)]
    hsClassModules = HS.writeClassModules inherit (M.elems (moduleClasses mod'))



main :: IO ()
main = do
  trees <- readModule jsonpath
  let pkgs = [toPackage trees]
  createDirectoryIfMissing True writepath

  mapM_ (writePackage writepath) pkgs
  mapM_ (\p -> sdist (writepath </> pModuleName p)) pkgs
  return ()


sdist :: String -> IO ()
sdist path = do
  (_,_,_,p0) <- createProcess $ (shell "cabal clean") { cwd = Just path }
  _ <- waitForProcess p0
  (_,_,_,p1) <- createProcess $ (shell "cabal configure") { cwd = Just path }
  _ <- waitForProcess p1
  (_,_,_,p2) <- createProcess $ (shell "cabal sdist") { cwd = Just path }
  _ <- waitForProcess p2

  (_,_,_,p3) <- createProcess $ (shell "mv dist/*.tar.gz ..") { cwd = Just path }
  _ <- waitForProcess p3

  return ()

baseClasses :: M.Map ClassType (S.Set ClassType) -> ClassType -> S.Set ClassType
baseClasses inheritance ct = go $ directDescendants inheritance ct
  where
    go :: S.Set ClassType -> S.Set ClassType
    go s0
      | s0 == s1 = s0
      | otherwise = go s1
      where
        s1 = S.unions $ s0 : map (directDescendants inheritance) (F.toList s0)

directDescendants :: M.Map ClassType (S.Set ClassType) -> ClassType -> S.Set ClassType
directDescendants inheritance ct = case M.lookup ct inheritance of
  Nothing -> S.empty
  Just xs -> S.filter (/= ct) xs
