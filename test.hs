{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Vector as V
import Casadi.Wrappers.SXFunction
import Casadi.Wrappers.Tools
import Casadi.Wrappers.SharedObject
import Casadi.Wrappers.IOInterfaceFX
import Casadi.Wrappers.FX
import Casadi.Wrappers.PrintableObject

main :: IO ()
main = do
  x <- ssym'' "x"
  f <- sXFunction''' (V.fromList [x]) (V.fromList [x])
  sharedObject_init' f
  iOInterfaceFX_setInput''' f (V.fromList [3]) 0
  fX_evaluate'' f
  out <- iOInterfaceFX_output f 0
  printableObject_getDescription out >>= putStrLn
  printableObject_getRepresentation out >>= putStrLn
