{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Casadi.Internal.Callback
       ( c_newCallbackHaskell
       , mkCallback
       , c_newCustomEvaluateHaskell
       , mkCustomEvaluate
       , c_newDerivativeGeneratorHaskell
       , mkDerivativeGenerator
       ) where


import Foreign.C.Types
import Foreign.Ptr ( Ptr, FunPtr )
--import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_ )

--import Casadi.Symbolic.Data
--import Casadi.Internal.Marshal ( withMarshal )
--import Casadi.Internal.WrapReturn ( WrapReturn(..) )

-- these guys all have type parameters because core depends on internal, not the other way around

type CasadiCallback' function = Ptr function -> IO CInt
foreign import ccall "wrapper" mkCallback
  :: CasadiCallback' function -> IO (FunPtr (CasadiCallback' function))
foreign import ccall safe "new_callback_haskell" c_newCallbackHaskell
  :: FunPtr (CasadiCallback' function) -> IO (Ptr callback)


type CasadiCustomEvaluate' customFunction = Ptr customFunction -> IO ()
foreign import ccall "wrapper" mkCustomEvaluate
  :: CasadiCustomEvaluate' customFunction -> IO (FunPtr (CasadiCustomEvaluate' customFunction))
foreign import ccall safe "new_custom_evaluate_haskell" c_newCustomEvaluateHaskell
  :: FunPtr (CasadiCustomEvaluate' customFunction) -> IO (Ptr customEvaluate)


type CasadiDerivativeGenerator' function = Ptr function -> CInt -> CInt -> IO (Ptr function)
foreign import ccall "wrapper" mkDerivativeGenerator
  :: CasadiDerivativeGenerator' function -> IO (FunPtr (CasadiDerivativeGenerator' function))
foreign import ccall safe "new_derivative_generator_haskell" c_newDerivativeGeneratorHaskell
  :: FunPtr (CasadiDerivativeGenerator' function) -> IO (Ptr derivativeGenerator)
