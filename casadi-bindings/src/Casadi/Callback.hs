{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Casadi.Callback
       ( makeCallback
       , makeCustomEvaluate
       , makeDerivativeGenerator
       ) where

import Foreign.C.Types ( CInt )
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( newForeignPtr_ )
import Foreign.ForeignPtr.Unsafe ( unsafeForeignPtrToPtr )

import Casadi.Core.Data
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
import Casadi.Internal.Callback ( mkCallback, c_newCallbackHaskell
                                , mkCustomEvaluate, c_newCustomEvaluateHaskell
                                , mkDerivativeGenerator, c_newDerivativeGeneratorHaskell
                                )

-- | add a callback to an NLPSolver
makeCallback :: (Function -> IO CInt) -> IO Callback
makeCallback callback = do
  -- safely wrap the callback into the C-friendly version
  let callback' :: Ptr Function' -> IO CInt
      callback' ptrFx = do
        foreignCFun <- newForeignPtr_ ptrFx
        callback (Function foreignCFun)

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCallback callback'

  -- create the callback object
  (c_newCallbackHaskell callbackFunPtr :: IO (Ptr Callback')) >>= wrapReturn


-- | add a callback to an NLPSolver
makeCustomEvaluate :: (CustomFunction -> IO ()) -> IO CustomEvaluate
makeCustomEvaluate callback = do
  -- safely wrap the callback into the C-friendly version
  let callback' :: Ptr CustomFunction' -> IO ()
      callback' ptrFx = do
        foreignCFun <- newForeignPtr_ ptrFx
        callback (CustomFunction foreignCFun)

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCustomEvaluate callback'

  -- create the callback object
  (c_newCustomEvaluateHaskell callbackFunPtr :: IO (Ptr CustomEvaluate')) >>= wrapReturn


-- | add a callback to an NLPSolver
makeDerivativeGenerator :: (Function -> Int -> Int -> IO Function) -> IO DerivativeGenerator
makeDerivativeGenerator callback = do
  -- safely wrap the callback into the C-friendly version
  let callback' :: Ptr Function' -> CInt -> CInt -> IO (Ptr Function')
      callback' ptrFx nfwd nadj = do
        foreignCFun <- newForeignPtr_ ptrFx
        Function fun <- callback (Function foreignCFun) (fromIntegral nfwd) (fromIntegral nadj)
        return (unsafeForeignPtrToPtr fun)

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkDerivativeGenerator callback'

  -- create the callback object
  (c_newDerivativeGeneratorHaskell callbackFunPtr :: IO (Ptr DerivativeGenerator')) >>= wrapReturn
