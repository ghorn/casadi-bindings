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
         slice__10,
         slice__2,
         slice__3,
         slice__4,
         slice__5,
         slice__6,
         slice__7,
         slice__8,
         slice__9,
         slice_all__0,
         slice_all__1,
         slice_all__2,
         slice_get_str__0,
         slice_get_str__1,
         slice_info,
         slice_is_scalar,
         slice_operator__equals,
         slice_operator__nequals,
         slice_scalar,
         slice_type_name,
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
  :: Ptr (Ptr StdString) -> CLLong -> CInt -> IO (Ptr Slice')

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
  :: Ptr (Ptr StdString) -> CLLong -> CInt -> CInt -> IO (Ptr Slice')

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
  :: Ptr (Ptr StdString) -> CInt -> CLLong -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__2
  :: Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice__2 :: Int -> Int -> IO Slice
slice__2 = casadi__Slice__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__3" c_casadi__Slice__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> CInt -> CLLong -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__3
  :: Int -> Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
slice__3 :: Int -> Int -> Int -> IO Slice
slice__3 = casadi__Slice__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__4" c_casadi__Slice__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__4
  :: Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice__4 :: Int -> Int -> IO Slice
slice__4 = casadi__Slice__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__5" c_casadi__Slice__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> CInt -> CInt -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__5
  :: Int -> Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
slice__5 :: Int -> Int -> Int -> IO Slice
slice__5 = casadi__Slice__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__6" c_casadi__Slice__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__6
  :: Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice__6 :: Int -> Int -> IO Slice
slice__6 = casadi__Slice__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__7" c_casadi__Slice__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> CLLong -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__7
  :: Int -> Int -> Int -> IO Slice
casadi__Slice__CONSTRUCTOR__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
slice__7 :: Int -> Int -> Int -> IO Slice
slice__7 = casadi__Slice__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__8" c_casadi__Slice__CONSTRUCTOR__8
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__8
  :: Int -> IO Slice
casadi__Slice__CONSTRUCTOR__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice__8 :: Int -> IO Slice
slice__8 = casadi__Slice__CONSTRUCTOR__8


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__9" c_casadi__Slice__CONSTRUCTOR__9
  :: Ptr (Ptr StdString) -> CLLong -> CInt -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__9
  :: Int -> Bool -> IO Slice
casadi__Slice__CONSTRUCTOR__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__9 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice__9 :: Int -> Bool -> IO Slice
slice__9 = casadi__Slice__CONSTRUCTOR__9


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__CONSTRUCTOR__10" c_casadi__Slice__CONSTRUCTOR__10
  :: Ptr (Ptr StdString) -> IO (Ptr Slice')

casadi__Slice__CONSTRUCTOR__10
  :: IO Slice
casadi__Slice__CONSTRUCTOR__10  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__CONSTRUCTOR__10 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
slice__10 :: IO Slice
slice__10 = casadi__Slice__CONSTRUCTOR__10


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__all__0" c_casadi__Slice__all__0
  :: Ptr (Ptr StdString) -> Ptr Slice' -> Ptr Slice' -> CLLong -> IO (Ptr (StdVec CLLong))

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
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CLLong -> IO (Ptr (StdVec CLLong))

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
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CLLong -> CInt -> IO (Ptr (StdVec CLLong))

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
foreign import ccall unsafe "casadi__Slice__get_str__0" c_casadi__Slice__get_str__0
  :: Ptr (Ptr StdString) -> Ptr Slice' -> IO (Ptr StdString)

casadi__Slice__get_str__0
  :: Slice -> IO String
casadi__Slice__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice_get_str__0 :: SliceClass a => a -> IO String
slice_get_str__0 x = casadi__Slice__get_str__0 (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__get_str__1" c_casadi__Slice__get_str__1
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CInt -> IO (Ptr StdString)

casadi__Slice__get_str__1
  :: Slice -> Bool -> IO String
casadi__Slice__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
slice_get_str__1 :: SliceClass a => a -> Bool -> IO String
slice_get_str__1 x = casadi__Slice__get_str__1 (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__info" c_casadi__Slice__info
  :: Ptr (Ptr StdString) -> Ptr Slice' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__Slice__info
  :: Slice -> IO (M.Map String GenericType)
casadi__Slice__info x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__info errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice_info :: SliceClass a => a -> IO (M.Map String GenericType)
slice_info x = casadi__Slice__info (castSlice x)


-- direct wrapper
foreign import ccall unsafe "casadi__Slice__is_scalar" c_casadi__Slice__is_scalar
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CLLong -> IO CInt

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
  :: Ptr (Ptr StdString) -> Ptr Slice' -> CLLong -> IO CLLong

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
foreign import ccall unsafe "casadi__Slice__type_name" c_casadi__Slice__type_name
  :: Ptr (Ptr StdString) -> Ptr Slice' -> IO (Ptr StdString)

casadi__Slice__type_name
  :: Slice -> IO String
casadi__Slice__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Slice__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
slice_type_name :: SliceClass a => a -> IO String
slice_type_name x = casadi__Slice__type_name (castSlice x)

