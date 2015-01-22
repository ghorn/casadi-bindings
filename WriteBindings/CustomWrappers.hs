{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.CustomWrappers
       ( function_custom_generateCode
       ) where

import Prelude hiding ( Functor )

import Foreign.C.Types
import Foreign.Marshal ( new, free )
import Foreign.Storable ( peek )
import Foreign.Ptr ( Ptr, nullPtr )

import Casadi.Internal.CToolsInstances ( )
import Casadi.Internal.FormatException ( formatException )
import Casadi.Internal.MarshalTypes ( StdString ) -- StdPair StdOstream'
import Casadi.Internal.Marshal ( withMarshal )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
import Casadi.Core.Data

-- direct wrapper
foreign import ccall unsafe "casadi__custom__generateCode" c_casadi__custom__generateCode
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr StdString)
casadi__custom__generateCode
  :: Function -> Bool -> IO String
casadi__custom__generateCode x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  do
    errStrPtrP <- new nullPtr
    ret <- c_casadi__custom__generateCode errStrPtrP x0' x1'
    errStrPtr <- peek errStrPtrP
    free errStrPtrP
    if errStrPtr == nullPtr then wrapReturn ret else wrapReturn errStrPtr >>= (error . formatException)


-- classy wrapper
function_custom_generateCode :: FunctionClass a => a -> Bool -> IO String
function_custom_generateCode x = casadi__custom__generateCode (castFunction x)
