{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.SharedObject
       (
         SharedObject,
         SharedObjectClass(..),
         sharedObject___hash__,
         sharedObject_class_name,
         sharedObject_get_str__0,
         sharedObject_get_str__1,
         sharedObject_is_null,
         sharedObject_print_ptr,
       ) where


import Prelude hiding ( Functor )

import Data.Vector ( Vector )
import qualified Data.Map as M
import Foreign.C.Types
import Foreign.Marshal ( new, free )
import Foreign.Storable ( peek )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.ForeignPtr ( newForeignPtr )
import System.IO.Unsafe ( unsafePerformIO ) -- for show instances

import Casadi.Internal.FormatException ( formatException )
import Casadi.Internal.MarshalTypes ( StdVec, StdString, StdMap, StdPair ) -- StdPair StdOstream'
import Casadi.Internal.Marshal ( Marshal(..), marshal, marshalFree )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )
import Casadi.Core.Data
import Casadi.Core.Enums
-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject____hash__" c_casadi__SharedObject____hash__
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO CLLong

casadi__SharedObject____hash__
  :: SharedObject -> IO Int
casadi__SharedObject____hash__ x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject____hash__ errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sharedObject___hash__ :: SharedObjectClass a => a -> IO Int
sharedObject___hash__ x = casadi__SharedObject____hash__ (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__class_name" c_casadi__SharedObject__class_name
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO (Ptr StdString)

casadi__SharedObject__class_name
  :: SharedObject -> IO String
casadi__SharedObject__class_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__class_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sharedObject_class_name :: SharedObjectClass a => a -> IO String
sharedObject_class_name x = casadi__SharedObject__class_name (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__get_str__0" c_casadi__SharedObject__get_str__0
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO (Ptr StdString)

casadi__SharedObject__get_str__0
  :: SharedObject -> IO String
casadi__SharedObject__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sharedObject_get_str__0 :: SharedObjectClass a => a -> IO String
sharedObject_get_str__0 x = casadi__SharedObject__get_str__0 (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__get_str__1" c_casadi__SharedObject__get_str__1
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> CInt -> IO (Ptr StdString)

casadi__SharedObject__get_str__1
  :: SharedObject -> Bool -> IO String
casadi__SharedObject__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sharedObject_get_str__1 :: SharedObjectClass a => a -> Bool -> IO String
sharedObject_get_str__1 x = casadi__SharedObject__get_str__1 (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__is_null" c_casadi__SharedObject__is_null
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO CInt

casadi__SharedObject__is_null
  :: SharedObject -> IO Bool
casadi__SharedObject__is_null x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__is_null errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sharedObject_is_null :: SharedObjectClass a => a -> IO Bool
sharedObject_is_null x = casadi__SharedObject__is_null (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__print_ptr" c_casadi__SharedObject__print_ptr
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO ()

casadi__SharedObject__print_ptr
  :: SharedObject -> IO ()
casadi__SharedObject__print_ptr x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__print_ptr errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sharedObject_print_ptr :: SharedObjectClass a => a -> IO ()
sharedObject_print_ptr x = casadi__SharedObject__print_ptr (castSharedObject x)

