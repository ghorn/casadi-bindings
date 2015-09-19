{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Function
       ( C.Function, AlwaysInline(..), NeverInline(..)
       , callMX, callMX', callSX, callSX', evalDMatrix, evalDMatrix'
       , jacobian, gradient, derivative
       , generateCode, externalFunction, externalFunction'
       ) where

import Data.Map ( Map )
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Classes.ExternalFunction as C
import qualified Casadi.Core.Classes.CodeGenerator as C
--import qualified Casadi.Core.CustomWrappers as C

import Casadi.SX ( SX )
import Casadi.MX ( MX )
import Casadi.DMatrix ( DMatrix )
import Casadi.GenericC ( GenericC ( mkGeneric ), Opt, GenericType )
import Casadi.SharedObject ( castSharedObject )

newtype AlwaysInline = AlwaysInline Bool
newtype NeverInline = NeverInline Bool

instance Show C.Function where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMX :: C.FunctionClass f => f -> Vector MX -> AlwaysInline -> NeverInline -> Vector MX
callMX f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO (C.function_operator_call__11 f ins alwaysInline neverInline)
{-# NOINLINE callMX #-}

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMX' :: C.FunctionClass f => f -> Map String MX -> AlwaysInline -> NeverInline -> Map String MX
callMX' f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO (C.function_operator_call__2 f ins alwaysInline neverInline)
{-# NOINLINE callMX' #-}

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: C.FunctionClass f => f -> Vector SX -> AlwaysInline -> NeverInline -> Vector SX
callSX f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO (C.function_operator_call__14 f ins alwaysInline neverInline)
{-# NOINLINE callSX #-}

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX' :: C.FunctionClass f => f -> Map String SX -> AlwaysInline -> NeverInline -> Map String SX
callSX' f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO (C.function_operator_call__5 f ins alwaysInline neverInline)
{-# NOINLINE callSX' #-}

-- | evaluate a Function with many inputs, many outputs
evalDMatrix :: C.FunctionClass f => f -> Vector DMatrix -> IO (Vector DMatrix)
evalDMatrix = C.function_operator_call__15

-- | evaluate a Function with many inputs, many outputs
evalDMatrix' :: C.FunctionClass f =>
                f -> Map String DMatrix -> IO (Map String DMatrix)
evalDMatrix' = C.function_operator_call__6

jacobian :: C.FunctionClass a => a -> Int -> Int -> Bool -> Bool -> IO C.Function
jacobian = C.function_jacobian__14

gradient :: C.FunctionClass a => a -> Int -> Int -> IO C.Function
gradient = C.function_gradient__6

derivative :: C.FunctionClass a => a -> Int -> Int -> IO C.Function
derivative = C.function_derivative

generateCode :: C.FunctionClass a => a -> Map String Opt -> String
generateCode f opts0 = unsafePerformIO $ do
  opts <- T.mapM mkGeneric opts0 :: IO (Map String GenericType)
  cg <- C.codeGenerator__1 opts
  C.codeGenerator_add__1 cg (C.castFunction f)
  C.codeGenerator_generate__0 cg
{-# NOINLINE generateCode #-}

externalFunction :: String -> Map String GenericType -> IO C.Function
externalFunction name opts = fmap C.castFunction $ C.externalFunction__5 name opts

externalFunction' :: String -> String -> Map String GenericType -> IO C.Function
externalFunction' name binName opts =
  fmap C.castFunction $ C.externalFunction__3 name binName opts
