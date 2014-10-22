{-# OPTIONS_GHC -Wall #-}

module Casadi.SXFunction
       ( C.SXFunction
       , sxFunction
       , sxFunctionFromFunction
       , sxFunctionFromMXFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.SXFunction as C
import qualified Casadi.Core.Classes.MXFunction as C
import Casadi.SX ( SX )

sxFunction :: Vector SX -> Vector SX -> IO C.SXFunction
sxFunction = C.sxFunction__0

sxFunctionFromFunction :: C.Function -> IO C.SXFunction
sxFunctionFromFunction = C.sxFunction__1

sxFunctionFromMXFunction :: C.MXFunction -> IO C.SXFunction
sxFunctionFromMXFunction = C.sxFunction__2
