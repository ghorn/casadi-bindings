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
         sharedObject_getDescription,
         sharedObject_getRepresentation,
         sharedObject_is_null,
         sharedObject_printPtr,
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
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO CSize

casadi__SharedObject____hash__
  :: SharedObject -> IO CSize
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
sharedObject___hash__ :: SharedObjectClass a => a -> IO CSize
sharedObject___hash__ x = casadi__SharedObject____hash__ (castSharedObject x)


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
foreign import ccall unsafe "casadi__SharedObject__printPtr" c_casadi__SharedObject__printPtr
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO ()

casadi__SharedObject__printPtr
  :: SharedObject -> IO ()
casadi__SharedObject__printPtr x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__printPtr errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sharedObject_printPtr :: SharedObjectClass a => a -> IO ()
sharedObject_printPtr x = casadi__SharedObject__printPtr (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__getRepresentation" c_casadi__SharedObject__getRepresentation
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO (Ptr StdString)

casadi__SharedObject__getRepresentation
  :: SharedObject -> IO String
casadi__SharedObject__getRepresentation x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__getRepresentation errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sharedObject_getRepresentation :: SharedObjectClass a => a -> IO String
sharedObject_getRepresentation x = casadi__SharedObject__getRepresentation (castSharedObject x)


-- direct wrapper
foreign import ccall unsafe "casadi__SharedObject__getDescription" c_casadi__SharedObject__getDescription
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO (Ptr StdString)

casadi__SharedObject__getDescription
  :: SharedObject -> IO String
casadi__SharedObject__getDescription x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SharedObject__getDescription errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sharedObject_getDescription :: SharedObjectClass a => a -> IO String
sharedObject_getDescription x = casadi__SharedObject__getDescription (castSharedObject x)

