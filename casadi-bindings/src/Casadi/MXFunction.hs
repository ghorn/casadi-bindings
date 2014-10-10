{-# OPTIONS_GHC -Wall #-}

module Casadi.MXFunction
       ( C.MXFunction, mxFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Core.Classes.MXFunction as C
import Casadi.MX ( MX )

mxFunction :: Vector MX -> Vector MX -> IO C.MXFunction
mxFunction = C.mxFunction__0
