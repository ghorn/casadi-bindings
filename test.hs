{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Vector as V
import Casadi.Wrappers.SXFunction
import Casadi.Wrappers.Tools
import Casadi.Wrappers.SharedObject
import Casadi.Wrappers.IOInterfaceFX
import Casadi.Wrappers.FX
import Casadi.Wrappers.PrintableObject
import Casadi.Wrappers.CasadiMeta

main :: IO ()
main = do
  putStr "casadiMeta_getBuildType: "
  casadiMeta_getBuildType >>= putStrLn

  putStr "casadiMeta_getCompiler: "
  casadiMeta_getCompiler >>= putStrLn

  putStr "casadiMeta_getCompilerFlags: "
  casadiMeta_getCompilerFlags >>= putStrLn

  putStr "casadiMeta_getCompilerId: "
  casadiMeta_getCompilerId >>= putStrLn

  putStr "casadiMeta_getFeatureList: "
  casadiMeta_getFeatureList >>= putStrLn

  putStr "casadiMeta_getGitDescribe: "
  casadiMeta_getGitDescribe >>= putStrLn

  putStr "casadiMeta_getGitRevision: "
  casadiMeta_getGitRevision >>= putStrLn

  putStr "casadiMeta_getVersion: "
  casadiMeta_getVersion >>= putStrLn

  putStrLn "---------------------------"
  x <- ssym'' "x"
  f <- sxFunction''' (V.fromList [x]) (V.fromList [x])
  sharedObject_init' f
  ioInterfaceFX_setInput''' f (V.fromList [3]) 0
  fx_evaluate'' f
  out <- ioInterfaceFX_output f 0
  printableObject_getDescription out >>= putStrLn
  printableObject_getRepresentation out >>= putStrLn

