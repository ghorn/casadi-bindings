{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}

module Casadi.Callback ( CasadiCallback
                       , addCallback
                       , fxSolveSafe
                       ) where


import Data.Vector ( Vector )
import Foreign.C.Types
import Foreign.Ptr --( Ptr )
import Foreign.ForeignPtr --( newForeignPtr )
--import qualified Foreign.Concurrent as FC

import Casadi.Wrappers.ForeignToolsInstances ( )
--import Casadi.Wrappers.Deleters
import Casadi.Wrappers.Data
import Casadi.Marshal ( Marshal(..) )
import Casadi.MarshalTypes ( CppVec )
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


type CasadiCallback' = Ptr CFunction' -> CInt -> CInt -> IO ()
foreign import ccall "wrapper" mkCallback :: CasadiCallback' -> IO (FunPtr CasadiCallback')

--foreign import ccall safe "new_cfunction_with_callback" c_newCFunctionWithCallback
--  :: FunPtr CasadiCallback' -> IO (Ptr CFunction')

foreign import ccall safe "add_iteration_callback" c_addIterationCallback
  :: Ptr (CppVec (Ptr CRSSparsity')) -> Ptr (CppVec (Ptr CRSSparsity')) ->
     FunPtr CasadiCallback' -> Ptr NLPSolver' -> IO ()

type CasadiCallback = CFunction -> Int -> Int -> IO ()

-- | add a callback to an NLPSolver
addCallback :: NLPSolverClass a => a -> CasadiCallback -> Vector CRSSparsity -> Vector CRSSparsity -> IO ()
addCallback nlpsolClass callback inputScheme outputScheme = do
  putStrLn "adding the callback"
  -- cast the nlpsolver
  let nlpsol :: NLPSolver
      nlpsol = castNLPSolver nlpsolClass :: NLPSolver
      NLPSolver nlpSolverForeignPtr = nlpsol

  -- safely wrap the callback into the C-friendly version
  let callback' :: CasadiCallback'
      callback' ptrCFun k0 k1 = do
        foreignCFun <- newForeignPtr_ ptrCFun
        callback (CFunction foreignCFun) (fromIntegral k0) (fromIntegral k1)

  -- turn the callback into a FunPtr
  putStrLn "making funptr"
  callbackFunPtr <- mkCallback callback' :: IO (FunPtr CasadiCallback')

  -- add the iteration callback
  putStrLn "adding iteration callback"
  withForeignPtr nlpSolverForeignPtr $ \nlp ->
    withMarshal inputScheme  $ \inputScheme'  ->
    withMarshal outputScheme $ \outputScheme' ->
    c_addIterationCallback inputScheme' outputScheme' callbackFunPtr nlp

  -- add a finalizer to the NLPSolver so that it frees the haskell FunPtr
  -- this is an unsafe solution if the NLPSolver is called after it is finalized
  --
  -- Apparently, this is illegal. I got this runtime error:
  -- > GHC.ForeignPtr: attempt to mix Haskell and C finalizers in the same ForeignPtr
  -- So comment this out and add a memory leak ;(
  -- FC.addForeignPtrFinalizer nlpSolverForeignPtr (freeHaskellFunPtr callbackFunPtr)
