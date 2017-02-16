{-# OPTIONS_GHC -Wall -fno-cse #-}

module Casadi.Function
       ( C.Function
       , externalFunction, externalFunction'
       , generateCode, generateCode'
       , callDM, callDM'
       , callV, callV'
       ) where

import           Control.Monad ( when, zipWithM, zipWithM_ )
import           Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Traversable as T
import           Foreign.C.Types ( CDouble, CInt(..) )
import           Foreign.ForeignPtr ( withForeignPtr )
import           Foreign.Ptr ( Ptr, nullPtr )
import           Foreign.Marshal.Alloc ( free )
import           Foreign.Marshal.Array ( mallocArray )
import           Foreign.Marshal.Utils ( new )
import           Foreign.Storable ( peekElemOff, pokeElemOff, peek )

import qualified Casadi.Core.Classes.CodeGenerator as C
import           Casadi.Core.Classes.DM ( DM )
import qualified Casadi.Core.Classes.Function as C
import           Casadi.Core.Data ( Function(..), Function' )
import qualified Casadi.Core.Tools as C
import           Casadi.Internal.FormatException ( formatException )
import           Casadi.Internal.MarshalTypes ( StdString )
import           Casadi.Internal.WrapReturn ( wrapReturn )

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


foreign import ccall safe "void hs_call_casadi_function_with_pointers" c_callV
  :: Ptr (Ptr StdString) -> Ptr Function'
  -> Ptr (Ptr CDouble) -> CInt
  -> Ptr (Ptr CDouble) -> CInt
  -> IO ()

callV :: C.Function -> Vector (Vector Double) -> IO (Vector (Vector Double))
callV f@(Function f') args = do
  -- check number of inputs
  nIn <- C.function_n_in f
  when (nIn /= V.length args) $
    error $ "callV: Function has " ++ show nIn ++ " inputs but you provided " ++ show (V.length args)

  -- check the size of each input
  inputSizes <- mapM (C.function_nnz_in__1 f) (take nIn [0..])
  let inputSizes' :: Vector Int
      inputSizes' = fmap V.length args

      checkInputSize k inputSize
        | inputSizes' V.! k == inputSize = return ()
        | otherwise = error $ "Function input " ++ show k ++ " has " ++ show inputSize
                      ++ " nonzeros but you provided " ++ show (inputSizes' V.! k)
  zipWithM_ checkInputSize [0..] inputSizes

  -- allocate the input buffer
  argBuffer <- mallocArray nIn :: IO (Ptr (Ptr CDouble))

  -- allocate and assign each input array
  let allocAndAssignArg k inputSize = do
        -- allocate input array
        argp <- mallocArray inputSize :: IO (Ptr CDouble)

        -- copy input to array
        let arg = args V.! k  :: Vector Double
        mapM_ (\j -> pokeElemOff argp j (realToFrac (arg V.! j))) (take inputSize [0..])

        -- assign input array to input buffer
        pokeElemOff argBuffer k argp

  zipWithM_ allocAndAssignArg (take nIn [0..]) inputSizes

  -- allocate output buffer
  nOut <- C.function_n_out f
  resBuffer <- mallocArray nOut :: IO (Ptr (Ptr CDouble))

  -- allocate memory for each output
  outputSizes <- mapM (C.function_nnz_out__1 f) (take nOut [0..])
  let allocRes k outputSize = do
        -- allocate output array
        resp <- mallocArray outputSize :: IO (Ptr CDouble)

        -- assign output array to output buffer
        pokeElemOff resBuffer k resp

  zipWithM_ allocRes [0..] outputSizes


  -- call the function
  errStrPtrP <- new nullPtr
  withForeignPtr f' $ \ff -> c_callV errStrPtrP ff argBuffer (fromIntegral nIn) resBuffer (fromIntegral nOut)
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  if errStrPtr /= nullPtr
    then wrapReturn errStrPtr >>= (error . formatException)
    else do

    -- deallocate the input vectors
    let freeInput k = peekElemOff argBuffer k >>= free
    mapM_ freeInput (take nIn [0..])

    -- deallocate the input buffer
    free argBuffer

    -- copy the outputs and free the output arrays
    let copyAndFreeOutput k outputSize = do
          resp <- peekElemOff resBuffer k
          res <- VM.new outputSize :: IO (VM.IOVector Double)
          let copyOutput j = peekElemOff resp j >>= (VM.write res j . realToFrac)
          mapM_ copyOutput (take outputSize [0..])
          -- free the output array
          free resp
          -- return the vector
          V.unsafeFreeze res

    ret <- zipWithM copyAndFreeOutput [0..] outputSizes

    -- free the output buffer
    free resBuffer

    return (V.fromList ret)


callV' :: C.Function -> M.Map String (Vector Double) -> IO (M.Map String (Vector Double))
callV' f inputMap = do
  funName <- C.function_name f
  inputNames <- C.function_name_in__1 f
  outputNames <- C.function_name_out__1 f

  let inputNamesSet = S.fromList (V.toList inputNames)
      inputMapKeysSet = M.keysSet inputMap
      missingInputs = S.difference inputNamesSet inputMapKeysSet
      extraInputs = S.difference inputMapKeysSet inputNamesSet

      describeBadInputs =
        error $ "callV': " ++ funName ++ ":\n" ++
        "expected inputs: " ++ show (V.toList inputNames) ++
        (if null missingInputs then "" else "\nmissing inputs: " ++ show missingInputs) ++
        (if null extraInputs then "" else "\nextra inputs: " ++ show extraInputs)

  when (M.size inputMap /= V.length inputNames) describeBadInputs

  let inputVec :: Vector (Vector Double)
      inputVec = fmap lookupInput inputNames
        where
          lookupInput inputName = case M.lookup inputName inputMap of
            Nothing -> describeBadInputs
            Just r -> r

  outputVec <- callV f inputVec

  when (V.length outputVec /= V.length outputNames) $
    error $ "callV': " ++ funName ++ ": something really weird happened, length of outputs "
      ++ show (V.length outputVec) ++ " /= length of names " ++ show (V.length outputNames)

  return $ M.fromList $ zip (V.toList outputNames) (V.toList outputVec)
