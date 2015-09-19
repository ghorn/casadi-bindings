{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Casadi.IOScheme
       ( InputOutputScheme(..)
       , mxFunctionWithSchemes
       , sxFunctionWithSchemes
       ) where

import Control.Monad ( forM, unless )
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
import Casadi.MXFunction ( mxFunction )
import Casadi.Option ( Opt )
import Casadi.SX ( SX )
import Casadi.SXFunction ( sxFunction )

mxFunctionWithSchemes ::
  String
  -> (InputOutputScheme, M.Map String MX)
  -> (InputOutputScheme, M.Map String MX)
  -> M.Map String Opt
  -> IO MXFunction
mxFunctionWithSchemes name (inputScheme, inputs) (outputScheme, outputs) opts = do
  inputVector <- schemeToVector inputScheme inputs
  outputVector <- schemeToVector outputScheme outputs
  mxFunction name inputVector outputVector opts

sxFunctionWithSchemes ::
  String
  -> (InputOutputScheme, M.Map String SX)
  -> (InputOutputScheme, M.Map String SX)
  -> M.Map String Opt
  -> IO SXFunction
sxFunctionWithSchemes name (inputScheme, inputs) (outputScheme, outputs) opts = do
  inputVector <- schemeToVector inputScheme inputs
  outputVector <- schemeToVector outputScheme outputs
  sxFunction name inputVector outputVector opts

schemeToVector ::
  forall a
  . Num a
  => InputOutputScheme -> M.Map String a -> IO (Vector a)
schemeToVector scheme symMap = do
  len <- C.getSchemeSize scheme
  let indices = take len [0..]
  
  indicesNames' <- forM indices $ \k -> do
    name <- C.getSchemeEntryName scheme k
    return (k, name)
  let indicesNames = M.fromList indicesNames' :: M.Map Int String
      namesSet = S.fromList (map snd indicesNames')
      extraEntries = M.keysSet symMap S.\\ namesSet

  _ <- unless (S.null extraEntries) $ error $ printf
    "case 1! scheme %s has entries %s, but you gave keys %s\n"
    (show scheme) (show namesSet) (show extraEntries)

  let lookup' :: Int -> a
      lookup' k = case M.lookup k indicesNames of
        Nothing -> error $ "schemeToVector: the impossible happened! scheme "
                   ++ show scheme ++ "has no " ++ show k ++ "-th entry"
        Just name -> case M.lookup name symMap of
          Nothing -> 0
          Just r -> r
  
  return $ V.fromList (map lookup' indices)
