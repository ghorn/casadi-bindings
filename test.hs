{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Vector as V
import Casadi.Wrappers.SXFunction
import Casadi.Wrappers.Tools
import Casadi.Wrappers.SharedObject

main :: IO ()
main = do
  x <- ssym'' "x"
  f <- sXFunction''' (V.fromList [x]) (V.fromList [x])
  sharedObject_init' f
  return ()
