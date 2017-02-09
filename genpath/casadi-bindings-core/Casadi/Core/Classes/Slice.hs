{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Slice
       (
         Slice,
         SliceClass(..),
         slice__0,
         slice__1,
         slice__2,
         slice__3,
         slice__4,
         slice_all__0,
         slice_all__1,
         slice_all__2,
         slice_getDescription,
         slice_getRepresentation,
         slice_is_scalar,
         slice_operator__equals,
         slice_operator__nequals,
         slice_scalar,
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
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__0" c_casadi__Slice__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__0
  :: Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice__0 :: Int -> Int -> IO Slice
slice__0 = casadi__Slice__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__1" c_casadi__Slice__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> CInt -> CInt -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__1
  :: Int -> Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
slice__1 :: Int -> Int -> Int -> IO Slice
slice__1 = casadi__Slice__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__2" c_casadi__Slice__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__2
  :: Int -> IO Slice
casadi__Slice__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice__2 :: Int -> IO Slice
slice__2 = casadi__Slice__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__3" c_casadi__Slice__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__3
  :: Int -> Bool -> IO Slice
casadi__Slice__CONSTRUCTOR__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice__3 :: Int -> Bool -> IO Slice
slice__3 = casadi__Slice__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__4" c_casadi__Slice__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__4
  :: IO Slice
casadi__Slice__CONSTRUCTOR__4  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__4 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
slice__4 :: IO Slice
slice__4 = casadi__Slice__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__all__0" c_casadi__Slice__all__0
  :: Ptr (Ptr StdString) -> Ptr Slice' -> Ptr Slice' -> CInt -> IO (Ptr (StdVec CInt))

casadi__Slice__all__0
  :: Slice -> Slice -> Int -> IO (Vector Int)
casadi__Slice__all__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__all__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
slice_all__0 :: SliceClass a => a -> Slice -> Int -> IO (Vector Int)
slice_all__0 x = casadi__Slice__all__0 (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__all__1" c_casadi__Slice__all__1
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CInt -> IO (Ptr (StdVec CInt))

casadi__Slice__all__1
  :: Slice -> Int -> IO (Vector Int)
casadi__Slice__all__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__all__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice_all__1 :: SliceClass a => a -> Int -> IO (Vector Int)
slice_all__1 x = casadi__Slice__all__1 (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__all__2" c_casadi__Slice__all__2
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CInt -> CInt -> IO (Ptr (StdVec CInt))

casadi__Slice__all__2
  :: Slice -> Int -> Bool -> IO (Vector Int)
casadi__Slice__all__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__all__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
slice_all__2 :: SliceClass a => a -> Int -> Bool -> IO (Vector Int)
slice_all__2 x = casadi__Slice__all__2 (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__is_scalar" c_casadi__Slice__is_scalar
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CInt -> IO CInt

casadi__Slice__is_scalar
  :: Slice -> Int -> IO Bool
casadi__Slice__is_scalar x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__is_scalar errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice_is_scalar :: SliceClass a => a -> Int -> IO Bool
slice_is_scalar x = casadi__Slice__is_scalar (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__operator__nequals" c_casadi__Slice__operator__nequals
  :: Ptr (Ptr StdString) -> Ptr Slice' -> Ptr Slice' -> IO CInt

casadi__Slice__operator__nequals
  :: Slice -> Slice -> IO Bool
casadi__Slice__operator__nequals x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__operator__nequals errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice_operator__nequals :: SliceClass a => a -> Slice -> IO Bool
slice_operator__nequals x = casadi__Slice__operator__nequals (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__operator__equals" c_casadi__Slice__operator__equals
  :: Ptr (Ptr StdString) -> Ptr Slice' -> Ptr Slice' -> IO CInt

casadi__Slice__operator__equals
  :: Slice -> Slice -> IO Bool
casadi__Slice__operator__equals x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__operator__equals errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice_operator__equals :: SliceClass a => a -> Slice -> IO Bool
slice_operator__equals x = casadi__Slice__operator__equals (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__scalar" c_casadi__Slice__scalar
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CInt -> IO CInt

casadi__Slice__scalar
  :: Slice -> Int -> IO Int
casadi__Slice__scalar x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__scalar errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice_scalar :: SliceClass a => a -> Int -> IO Int
slice_scalar x = casadi__Slice__scalar (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__getRepresentation" c_casadi__Slice__getRepresentation
  :: Ptr (Ptr StdString) -> Ptr Slice' -> IO (Ptr StdString)

casadi__Slice__getRepresentation
  :: Slice -> IO String
casadi__Slice__getRepresentation x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__getRepresentation errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice_getRepresentation :: SliceClass a => a -> IO String
slice_getRepresentation x = casadi__Slice__getRepresentation (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__getDescription" c_casadi__Slice__getDescription
  :: Ptr (Ptr StdString) -> Ptr Slice' -> IO (Ptr StdString)

casadi__Slice__getDescription
  :: Slice -> IO String
casadi__Slice__getDescription x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__getDescription errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice_getDescription :: SliceClass a => a -> IO String
slice_getDescription x = casadi__Slice__getDescription (castSlice x)

