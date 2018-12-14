{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.WeakRef
       (
         WeakRef,
         WeakRefClass(..),
         weakRef__0,
         weakRef__1,
         weakRef__2,
         weakRef_alive,
         weakRef_shared,
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
foreign import ccall unsafe "casadi__WeakRef__CONSTRUCTOR__0" c_casadi__WeakRef__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr SharedObject' -> IO (Ptr WeakRef')

casadi__WeakRef__CONSTRUCTOR__0
  :: SharedObject -> IO WeakRef
casadi__WeakRef__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__WeakRef__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
weakRef__0 :: SharedObject -> IO WeakRef
weakRef__0 = casadi__WeakRef__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__WeakRef__CONSTRUCTOR__1" c_casadi__WeakRef__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> IO (Ptr WeakRef')

casadi__WeakRef__CONSTRUCTOR__1
  :: IO WeakRef
casadi__WeakRef__CONSTRUCTOR__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__WeakRef__CONSTRUCTOR__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
weakRef__1 :: IO WeakRef
weakRef__1 = casadi__WeakRef__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__WeakRef__CONSTRUCTOR__2" c_casadi__WeakRef__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr WeakRef')

casadi__WeakRef__CONSTRUCTOR__2
  :: Int -> IO WeakRef
casadi__WeakRef__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__WeakRef__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
weakRef__2 :: Int -> IO WeakRef
weakRef__2 = casadi__WeakRef__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__WeakRef__alive" c_casadi__WeakRef__alive
  :: Ptr (Ptr StdString) -> Ptr WeakRef' -> IO CInt

casadi__WeakRef__alive
  :: WeakRef -> IO Bool
casadi__WeakRef__alive x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__WeakRef__alive errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
weakRef_alive :: WeakRefClass a => a -> IO Bool
weakRef_alive x = casadi__WeakRef__alive (castWeakRef x)


-- direct wrapper
foreign import ccall unsafe "casadi__WeakRef__shared" c_casadi__WeakRef__shared
  :: Ptr (Ptr StdString) -> Ptr WeakRef' -> IO (Ptr SharedObject')

casadi__WeakRef__shared
  :: WeakRef -> IO SharedObject
casadi__WeakRef__shared x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__WeakRef__shared errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
weakRef_shared :: WeakRefClass a => a -> IO SharedObject
weakRef_shared x = casadi__WeakRef__shared (castWeakRef x)

