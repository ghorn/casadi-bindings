{-# OPTIONS_GHC -Wall #-}

module Casadi.Callback ( makeCallback
                       , functionSolveSafe
                       ) where


import Foreign.C.Types
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr ( newForeignPtr_ )

import Casadi.Wrappers.CToolsInstances ( )
import Casadi.Wrappers.Data
import Casadi.Marshal ( withMarshal )
import Casadi.WrapReturn ( WrapReturn(..) )

-- direct wrapper to a safe version of "solve"
foreign import ccall safe "CasADi__Function__solve" c_CasADi__Function__solve_safe
  :: Ptr Function' -> IO ()

casADi__Function__solve_safe :: Function -> IO ()
casADi__Function__solve_safe x0 =
  withMarshal x0 $ \x0' ->
  c_CasADi__Function__solve_safe x0' >>= wrapReturn

-- | safe version of solve
functionSolveSafe :: FunctionClass a => a -> IO ()
functionSolveSafe x = casADi__Function__solve_safe (castFunction x)

type CasadiCallback' = Ptr Function' -> IO CInt
foreign import ccall "wrapper" mkCallback :: CasadiCallback' -> IO (FunPtr CasadiCallback')

foreign import ccall safe "new_callback_haskell" c_newCallbackHaskell
  :: FunPtr CasadiCallback' -> IO (Ptr Callback')

-- | add a callback to an NLPSolver
makeCallback :: (Function -> IO CInt) -> IO Callback
makeCallback callback = do
  -- safely wrap the callback into the C-friendly version
  let callback' :: CasadiCallback'
      callback' ptrFx = do
        foreignCFun <- newForeignPtr_ ptrFx
        callback (Function foreignCFun)

  -- turn the callback into a FunPtr
  callbackFunPtr <- mkCallback callback' :: IO (FunPtr CasadiCallback')

  -- create the callback object
  c_newCallbackHaskell callbackFunPtr >>= wrapReturn
