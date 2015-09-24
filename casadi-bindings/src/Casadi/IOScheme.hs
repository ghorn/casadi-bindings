{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Casadi.IOScheme
       ( InputOutputScheme(..)
       , mxFunctionWithSchemes
       , sxFunctionWithSchemes
       ) where

import Control.Monad ( unless )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Text.Printf ( printf )

import Casadi.Core.Classes.SXFunction
import Casadi.Core.Classes.MXFunction
import Casadi.Core.Enums ( InputOutputScheme(..) )
import qualified Casadi.Core.Tools as C

import Casadi.MX ( MX )
import Casadi.MXFunction ( mxFunction' )
import Casadi.Option ( Opt )
import Casadi.SX ( SX )
import Casadi.SXFunction ( sxFunction' )

mxFunctionWithSchemes ::
  String
  -> (InputOutputScheme, M.Map String MX)
  -> (InputOutputScheme, M.Map String MX)
  -> M.Map String Opt
  -> IO MXFunction
mxFunctionWithSchemes name (inputScheme, inputs) (outputScheme, outputs) opts = do
  input' <- schemeToMapVec inputScheme inputs
  output' <- schemeToMapVec outputScheme outputs
  mxFunction' name input' output' opts

sxFunctionWithSchemes ::
  String
  -> (InputOutputScheme, M.Map String SX)
  -> (InputOutputScheme, M.Map String SX)
  -> M.Map String Opt
  -> IO SXFunction
sxFunctionWithSchemes name (inputScheme, inputs) (outputScheme, outputs) opts = do
  input' <- schemeToMapVec inputScheme inputs
  output' <- schemeToMapVec outputScheme outputs
  sxFunction' name input' output' opts

schemeToMapVec ::
  forall a
  . Num a
  => InputOutputScheme -> M.Map String a -> IO (M.Map String a, Vector String)
schemeToMapVec scheme symMap = do
  len <- C.getSchemeSize scheme
  let indices = take len [0..]
  
  indicesNames <- mapM (C.getSchemeEntryName scheme) indices :: IO [String]

  let namesSet = S.fromList indicesNames
      extraEntries = M.keysSet symMap S.\\ namesSet

  _ <- unless (S.null extraEntries) $ error $ printf
    "scheme %s has entries %s, but you gave keys %s\n"
    (show scheme) (show namesSet) (show extraEntries)

  return (symMap, V.fromList indicesNames)
