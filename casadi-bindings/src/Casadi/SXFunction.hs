{-# OPTIONS_GHC -Wall #-}

module Casadi.SXFunction
       ( C.SXFunction, sxFunction
       ) where

import Data.Vector ( Vector )

import qualified Casadi.Core.Classes.SXFunction as C
import Casadi.SX ( SX )

sxFunction :: Vector SX -> Vector SX -> IO C.SXFunction
sxFunction = C.sxFunction__0
