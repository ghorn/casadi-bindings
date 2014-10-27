{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}

module Casadi.MXFunction
       ( C.MXFunction
       , mxFunction
       , mxFunctionFromFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.MXFunction as C

import Casadi.MX ( MX )
import Casadi.SharedObject ( castSharedObject )

instance Show C.MXFunction where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

mxFunction :: Vector MX -> Vector MX -> IO C.MXFunction
mxFunction = C.mxFunction__0

mxFunctionFromFunction :: C.Function -> IO C.MXFunction
mxFunctionFromFunction = C.mxFunction__1
