{-# OPTIONS_GHC -Wall -fno-cse #-}

module Casadi.Interpolant
       ( interpolant
       ) where

import Data.Map ( Map )
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Casadi.Core.Tools as C

import Casadi.GenericType ( GenericType, GType, fromGType )
import Casadi.Function ( Function )

interpolant :: String -> String -> Vector (Vector Double) -> Vector Double -> Map String GType -> Function
interpolant name solver grid values opts0 = unsafePerformIO $ do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.interpolant__1 name solver grid values opts
