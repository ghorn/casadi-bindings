{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}

module Casadi.Internal.Callback
       ( c_newCallbackHaskell
       , mkCallback
       ) where


import Foreign.C.Types
import Foreign.Ptr ( Ptr, FunPtr )
--import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_ )

--import Casadi.Symbolic.Data
--import Casadi.Internal.Marshal ( withMarshal )
--import Casadi.Internal.WrapReturn ( WrapReturn(..) )

type CasadiCallback' function' = Ptr function' -> IO CInt
foreign import ccall "wrapper" mkCallback :: CasadiCallback' function' -> IO (FunPtr (CasadiCallback' function'))

foreign import ccall safe "new_callback_haskell" c_newCallbackHaskell
  :: FunPtr (CasadiCallback' function') -> IO (Ptr callback')
