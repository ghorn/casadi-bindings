{-# OPTIONS_GHC -Wall -fno-cse #-}

module Casadi.Function
       ( C.Function
       , externalFunction, externalFunction'
       , generateCode, generateCode'
       , callDM, callDM'
       ) where

import           Data.Map ( Map )
import           Data.Vector ( Vector )
import qualified Data.Traversable as T

import qualified Casadi.Core.Classes.CodeGenerator as C
import           Casadi.Core.Classes.DM ( DM )
import qualified Casadi.Core.Classes.Function as C
import qualified Casadi.Core.Tools as C

import Casadi.GenericType ( GenericType, GType, fromGType )

generateCode :: C.Function -> String -> Map String GType -> IO String
generateCode f n opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.function_generate__3 f n opts

generateCode' :: C.Function -> String -> Map String GType -> IO String
generateCode' f name opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  cg <- C.codeGenerator__1 name opts
  C.codeGenerator_add cg f
  C.codeGenerator_generate__0 cg

externalFunction :: String -> Map String GType -> IO C.Function
externalFunction name opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.external__5 name opts

externalFunction' :: String -> String -> Map String GType -> IO C.Function
externalFunction' name binName opts0 = do
  opts <- T.mapM fromGType opts0 :: IO (Map String GenericType)
  C.external__3 name binName opts

callDM :: C.Function -> Vector DM -> IO (Vector DM)
callDM f ins = C.function_call__15 f ins

callDM' :: C.Function -> Map String DM -> IO (Map String DM)
callDM' f ins = C.function_call__6 f ins
