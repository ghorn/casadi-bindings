{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}

module Casadi.MXFunction
       ( C.MXFunction
       , mxFunction
       , mxFunctionFromFunction
       ) where

import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Data.Map ( Map )

import qualified Casadi.Core.Classes.Function as C
import Casadi.Core.Classes.GenericType ( GenericType )
import qualified Casadi.Core.Classes.MXFunction as C

import Casadi.MX ( MX )
import Casadi.GenericC ( GenericC( mkGeneric ), Opt )
import Casadi.SharedObject ( castSharedObject )

instance Show C.MXFunction where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

mxFunction :: String -> Vector MX -> Vector MX -> Map String Opt
              -> IO C.MXFunction
mxFunction n x y opts0 = do
  opts <- T.mapM mkGeneric opts0 :: IO (Map String GenericType)
  C.mxFunction__1 n x y opts

mxFunctionFromFunction :: C.Function -> IO C.MXFunction
mxFunctionFromFunction = C.mxFunction__2
