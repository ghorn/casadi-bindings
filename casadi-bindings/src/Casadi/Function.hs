{-# OPTIONS_GHC -Wall -fno-cse -fno-warn-orphans #-}

module Casadi.Function
       ( C.Function, AlwaysInline(..), NeverInline(..)
       , mxFunction, mxFunction', sxFunction, sxFunction'
       , callMX, callMX', callSX, callSX', callDM, callDM'
       , hessian, jacobian, gradient
       , externalFunction, externalFunction'
       , generateCode, generateCode'
       ) where

import Data.Map ( Map )
import qualified Data.Traversable as T
import Data.Vector ( Vector )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Casadi.Core.Classes.CodeGenerator as C
import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Tools as C

import Casadi.SX ( SX )
import Casadi.MX ( MX )
import Casadi.DM ( DM )
import Casadi.GenericType ( GenericType, GType, fromGType )

newtype AlwaysInline = AlwaysInline Bool
newtype NeverInline = NeverInline Bool

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMX :: C.Function -> Vector MX -> AlwaysInline -> NeverInline -> Vector MX
callMX f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO $ C.function_call__11 f ins alwaysInline neverInline

-- | call an MXFunction on symbolic inputs, getting symbolic outputs
callMX' :: C.Function -> Map String MX -> AlwaysInline -> NeverInline -> Map String MX
callMX' f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO $ C.function_call__2 f ins alwaysInline neverInline

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX :: C.Function -> Vector SX -> AlwaysInline -> NeverInline -> Vector SX
callSX f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO $ C.function_call__14 f ins alwaysInline neverInline

-- | call an SXFunction on symbolic inputs, getting symbolic outputs
callSX' :: C.Function -> Map String SX -> AlwaysInline -> NeverInline -> Map String SX
callSX' f ins (AlwaysInline alwaysInline) (NeverInline neverInline) =
  unsafePerformIO $ C.function_call__5 f ins alwaysInline neverInline

-- TODO(greg): expose the thread safe way
-- | evaluate a Function with many inputs, many outputs
callDM :: C.Function -> Vector DM -> IO (Vector DM)
callDM f ins = C.function_call__15 f ins

-- | evaluate a Function with many inputs, many outputs
callDM' :: C.FunctionClass f => f -> Map String DM -> IO (Map String DM)
callDM' f ins = C.function_call__6 f ins

hessian :: C.FunctionClass a => a -> Int -> Int -> IO C.Function
hessian = C.function_hessian__6

jacobian :: C.FunctionClass a => a -> Int -> Int -> Bool -> Bool -> IO C.Function
jacobian = C.function_jacobian__14

gradient :: C.FunctionClass a => a -> Int -> Int -> IO C.Function
gradient = C.function_gradient__6

generateCode :: C.FunctionClass a => a -> String -> Map String GType -> IO String
generateCode f n opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.function_generate__3 (C.castFunction f) n opts

generateCode' :: C.FunctionClass a => a -> String -> Map String GType -> IO String
generateCode' f name opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  cg <- C.codeGenerator__1 name opts
  C.codeGenerator_add cg (C.castFunction f)
  C.codeGenerator_generate__0 cg

externalFunction :: String -> Map String GenericType -> IO C.Function
externalFunction name opts = C.external__5 name opts

externalFunction' :: String -> String -> Map String GenericType -> IO C.Function
externalFunction' name binName opts =
  fmap C.castFunction $ C.external__3 name binName opts

sxFunction :: String -> Vector SX -> Vector SX -> Map String GType
              -> IO C.Function
sxFunction n x y opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.function__11 n x y opts

sxFunction' ::
  String -> (Vector SX, Vector String) -> (Vector SX, Vector String)
  -> Map String GType
  -> IO C.Function
sxFunction' n (x,nx) (y,ny) opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.function__9 n x y nx ny opts

--sxFunctionFromFunction :: C.Function -> IO C.SXFunction
--sxFunctionFromFunction = C.sxFunction__8

--sxFunctionFromMXFunction :: C.MXFunction -> IO C.SXFunction
--sxFunctionFromMXFunction = C.sxFunction__9

mxFunction :: String -> Vector MX -> Vector MX -> Map String GType
              -> IO C.Function
mxFunction n x y opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.function__5 n x y opts

mxFunction' ::
  String -> (Vector MX, Vector String) -> (Vector MX, Vector String)
  -> Map String GType
  -> IO C.Function
mxFunction' n (x,nx) (y,ny) opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.function__3 n x y nx ny opts

--mxFunctionFromFunction :: C.Function -> IO C.MXFunction
--mxFunctionFromFunction = C.mxFunction__8
