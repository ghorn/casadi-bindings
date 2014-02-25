{-# OPTIONS_GHC -Wall #-}

module Casadi.Callback ( makeCallback
                       , fxSolveSafe
                       ) where


import Foreign.C.Types
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr ( newForeignPtr_ )

import Casadi.Wrappers.CToolsInstances ( )
import Casadi.Wrappers.Data
import Casadi.Marshal ( withMarshal )
import Casadi.WrapReturn ( WrapReturn(..) )

-- direct wrapper to a safe version of "solve"
foreign import ccall safe "CasADi__FX__solve" c_CasADi__FX__solve_safe
  :: Ptr FX' -> IO ()

casADi__FX__solve_safe :: FX -> IO ()
casADi__FX__solve_safe x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__FX__solve_safe x0' >>= wrapReturn

-- | safe version of solve
fxSolveSafe :: FXClass a => a -> IO ()
fxSolveSafe x = casADi__FX__solve_safe (castFX x)

type CasadiCallback' = Ptr FX' -> IO CInt
foreign import ccall "wrapper" mkCallback :: CasadiCallback' -> IO (FunPtr CasadiCallback')

foreign import ccall safe "new_callback_haskell" c_newCallbackHaskell
  :: FunPtr CasadiCallback' -> IO (Ptr Callback')

-- | add a callback to an NLPSolver
makeCallback :: (FX -> IO CInt) -> IO Callback
makeCallback callback = do
  -- safely wrap the callback into the C-friendly version
  let callback' :: CasadiCallback'
      callback' ptrFx = do
        foreignCFun <- newForeignPtr_ ptrFx
        callback (FX foreignCFun)

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCallback callback' :: IO (FunPtr CasadiCallback')

  -- create the callback object
  c_newCallbackHaskell callbackFunPtr >>= wrapReturn
