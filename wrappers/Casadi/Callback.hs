{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module Casadi.Callback ( makeCallback
                       , fxSolveSafe
                       ) where


import Foreign.C.Types
import Foreign.Ptr --( Ptr )
import Foreign.ForeignPtr --( newForeignPtr )
--import qualified Foreign.Concurrent as FC

import Casadi.Wrappers.ForeignToolsInstances ( )
--import Casadi.Wrappers.Deleters
import Casadi.Wrappers.Data
import Casadi.Marshal ( Marshal(..) )
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

foreign import ccall safe "&delete_callback_haskell" c_deleteCallbackHaskell
  :: FunPtr (Ptr Callback' -> IO ())

-- | add a callback to an NLPSolver
makeCallback :: (FX -> IO CInt) -> IO Callback
makeCallback callback = do
  putStrLn "adding the callback"

  -- safely wrap the callback into the C-friendly version
  let callback' :: CasadiCallback'
      callback' ptrFx = do
        foreignCFun <- newForeignPtr_ ptrFx
        callback (FX foreignCFun)

  -- turn the callback into a FunPtr
  putStrLn "making funptr"
  callbackFunPtr <- mkCallback callback' :: IO (FunPtr CasadiCallback')

  c_newCallbackHaskell callbackFunPtr >>= (newForeignPtr c_deleteCallbackHaskell) >>= wrapReturn

  -- add a finalizer to the NLPSolver so that it frees the haskell FunPtr
  -- this is an unsafe solution if the NLPSolver is called after it is finalized
  --
  -- Apparently, this is illegal. I got this runtime error:
  -- > GHC.ForeignPtr: attempt to mix Haskell and C finalizers in the same ForeignPtr
  -- So comment this out and add a memory leak ;(
  -- FC.addForeignPtrFinalizer nlpSolverForeignPtr (freeHaskellFunPtr callbackFunPtr)
