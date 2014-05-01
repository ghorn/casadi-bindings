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
import qualified WriteBindings.TypeMaps as TM
import System.Process ( CreateProcess(..), createProcess, shell, waitForProcess )

version :: String
version = "1.9.0.1"

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
  , pCClasses :: (FilePath,String)
  , pCFunctions :: (FilePath,String)
  , pHsClasses :: [(FilePath,String)]
  , pHsTools :: Maybe (FilePath,String)
  , pHsEnums :: Maybe (FilePath,String)
  , pHsData :: (FilePath,String)
  , pModuleName :: String
  , pHSModuleName :: String
  }

writePackage :: FilePath -> Package -> IO ()
writePackage rootDir pkg = do
  putStrLn $ "writing " ++ (pModuleName pkg)
  let wf :: (FilePath, String) -> IO ()
      wf (fp,contents) = writeFile' (rootDir </> pModuleName pkg </> fp) contents

  license <- readFile (rootDir </> ".." </> "casadi-bindings-internal" </> "LICENSE")
  marshallHpp <- readFile (rootDir </> ".." </> "WriteBindings" </> "marshal.hpp")

  wf ("LICENSE",license)
  wf ("cbits" </> "marshal.hpp",marshallHpp)
  wf (pCabal pkg)
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
  { pCClasses = ("cbits/autogen/"++modname++"_classes.cpp",
                 unlines $ cincludes ++ [""] ++
                 map C.writeClass (M.elems (moduleClasses mod'))
                )
  , pCFunctions = ("cbits/autogen/"++modname++"_functions.cpp",
                 unlines $ cincludes ++ [""] ++
                 concatMap C.writeFunctions (moduleFunctions mod')
                )
  , pHsTools = case moduleFunctions mod' of
       [] -> Nothing
       xs -> Just ("Casadi/" ++ ucname ++ "/Tools.hs", HS.writeToolsModule ucname xs)
  , pHsClasses = map (first (\n -> "Casadi/" ++ ucname ++ "/Classes/" ++ n ++ ".hs")) hsClassModules
  , pHsEnums = case (M.toList (moduleEnums mod')) of
    --[] -> Nothing
    xs -> Just ("Casadi/" ++ ucname ++ "/Enums.hs", HS.writeEnumsModule ucname xs)
  , pModuleName = modname
  , pHSModuleName = hsmodname
  , pHsData = ("Casadi/" ++ ucname ++ "/Data.hs",
               HS.writeDataModule ucname allclasses inherit)
  , pCabal = ("casadi-bindings-" ++ hsmodname ++ ".cabal",
              unlines $
              [ "name:                casadi-bindings-" ++ hsmodname
              , "version:             " ++ version
              , "license:             LGPL-3"
              , "license-file:        LICENSE"
              , "copyright:           (c) 2013-2014 Greg Horn"
              , "author:              Greg Horn"
              , "maintainer:          gregmainland@gmail.com"
              , "build-type:          Simple"
              , "cabal-version:       >=1.10"
              , "synopsis:            low level bindings to CasADi"
              , "extra-source-files:  cbits/marshal.hpp"
              , "extra-tmp-files:     Casadi/Callback_stub.h"
              , ""
              , "library"
              , "  build-depends:       base >=4.6 && <5,"
              , "                       vector >=0.10,"
              , if modname == "core"
                then ""
                else "                       casadi-bindings-core,"
              , "                       casadi-bindings-internal"
              , ""
              , "  default-language:    Haskell2010"
              , ""
              , ""
              , "  extra-libraries:  stdc++"
              , ""
              , "  pkgconfig-depends: casadi_" ++ modname
              , ""
              , "  ghc-prof-options: -prof -fprof-auto -fprof-cafs -rtsopts"
              , "  ghc-options: "
              , "  cc-options: -Wall -Wno-delete-non-virtual-dtor"
              , ""
              , "  C-sources:        cbits/autogen/"++modname++"_classes.cpp"
              , "                    cbits/autogen/"++modname++"_functions.cpp"
              , ""
              , "  exposed-modules:  Casadi." ++ ucname ++ ".Data"
--              ] ++ (if M.size (moduleEnums mod') == 0
--                    then [] else ["                    Casadi." ++ ucname ++ ".Enums"])
              ] ++ ["                    Casadi." ++ ucname ++ ".Enums"]
              ++ case moduleFunctions mod' of
                     [] -> []
                     _ -> ["                    Casadi." ++ ucname ++ ".Tools"]
              ++ map (\(n,_) -> "                    " ++
                                "Casadi." ++ ucname ++ ".Classes." ++ uppercase n) hsClassModules
              )
  }
  where
    hsmodname = TM.replaces [("_","-")] modname
    inherit :: ClassType -> S.Set ClassType
    inherit = baseClasses (moduleInheritance mod')

    ucname = toUcname True modname
    toUcname True ('_':_) = error "toUcname True ('_':_)"
    toUcname False ('_':xs) = toUcname True xs
    toUcname True (x:xs) = toUpper x : toUcname False xs
    toUcname False (x:xs) = x : toUcname False xs
    toUcname _ [] = []
    
    modname = moduleName mod'
    cincludes :: [String]
    cincludes = moduleIncludes mod' ++ [ "#include \"../marshal.hpp\"" ]

    allclasses = M.elems (moduleClasses mod')

    hsClassModules :: [(String,String)]
    hsClassModules = HS.writeClassModules ucname inherit (M.elems (moduleClasses mod'))


main :: IO ()
main = do
  let rootpath = "/home/ghorn/casadi/build/swig"
  trees <- readModules rootpath
  let writepath = "/home/ghorn/hslibs/casadi-bindings/genpath"
      pkgs = map toPackage trees
  createDirectoryIfMissing True writepath

  mapM_ (writePackage writepath) pkgs
  mapM_ (\p -> sdist (writepath </> pModuleName p)) pkgs

  writeFile' (writepath </> "unregister.sh")
    (unlines $ map (("ghc-pkg unregister casadi-bindings-" ++) . pHSModuleName) pkgs)
  return ()


sdist :: String -> IO ()
sdist path = do
  (_,_,_,p) <- createProcess $ (shell "cabal sdist") { cwd = Just path }
  _ <- waitForProcess p

  (_,_,_,p') <- createProcess $ (shell "mv dist/*.tar.gz ..") { cwd = Just path }
  _ <- waitForProcess p'

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
