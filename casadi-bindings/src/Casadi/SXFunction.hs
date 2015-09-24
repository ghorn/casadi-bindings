{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans#-}

module Casadi.SXFunction
       ( C.SXFunction
       , sxFunction
       , sxFunction'
       , sxFunctionFromFunction
       , sxFunctionFromMXFunction
       ) where

import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Data.Map ( Map )

import qualified Casadi.Core.Classes.Function as C
import Casadi.Core.Classes.GenericType ( GenericType )
import qualified Casadi.Core.Classes.SXFunction as C
import qualified Casadi.Core.Classes.MXFunction as C

import Casadi.GenericC ( GenericC( mkGeneric ), Opt )
import Casadi.SharedObject ( castSharedObject )
import Casadi.SX ( SX )

instance Show C.SXFunction where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

sxFunction :: String -> Vector SX -> Vector SX -> Map String Opt
              -> IO C.SXFunction
sxFunction n x y opts0 = do
  opts <- T.mapM mkGeneric opts0 :: IO (Map String GenericType)
  C.sxFunction__7 n x y opts

sxFunction' ::
  String -> (Map String SX, Vector String) -> (Map String SX, Vector String)
  -> Map String Opt
  -> IO C.SXFunction
sxFunction' n x y opts0 = do
  opts <- T.mapM mkGeneric opts0 :: IO (Map String GenericType)
  C.sxFunction__1 n x y opts

sxFunctionFromFunction :: C.Function -> IO C.SXFunction
sxFunctionFromFunction = C.sxFunction__8

sxFunctionFromMXFunction :: C.MXFunction -> IO C.SXFunction
sxFunctionFromMXFunction = C.sxFunction__9
