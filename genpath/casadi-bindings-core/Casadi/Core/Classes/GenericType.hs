{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.GenericType
       (
         GenericType,
         GenericTypeClass(..),
         genericType__0,
         genericType__1,
         genericType__10,
         genericType__11,
         genericType__12,
         genericType__13,
         genericType__14,
         genericType__15,
         genericType__2,
         genericType__3,
         genericType__4,
         genericType__5,
         genericType__6,
         genericType__7,
         genericType__8,
         genericType__9,
         genericType_as_bool,
         genericType_as_bool_vector,
         genericType_as_dict,
         genericType_as_double,
         genericType_as_double_vector,
         genericType_as_double_vector_vector,
         genericType_as_function,
         genericType_as_function_vector,
         genericType_as_int,
         genericType_as_int_vector,
         genericType_as_int_vector_vector,
         genericType_as_string,
         genericType_as_string_vector,
         genericType_as_void_pointer,
         genericType_getType,
         genericType_get_description,
         genericType_is_bool,
         genericType_is_bool_vector,
         genericType_is_dict,
         genericType_is_double,
         genericType_is_double_vector,
         genericType_is_double_vector_vector,
         genericType_is_empty_vector,
         genericType_is_function,
         genericType_is_function_vector,
         genericType_is_int,
         genericType_is_int_vector,
         genericType_is_int_vector_vector,
         genericType_is_string,
         genericType_is_string_vector,
         genericType_is_void_pointer,
         genericType_operator__equals,
         genericType_operator__nequals,
         genericType_to_bool,
         genericType_to_bool_vector,
         genericType_to_dict,
         genericType_to_double,
         genericType_to_double_vector,
         genericType_to_double_vector_vector,
         genericType_to_function,
         genericType_to_function_vector,
         genericType_to_int,
         genericType_to_int_type_vector,
         genericType_to_int_vector,
         genericType_to_int_vector_vector,
         genericType_to_string,
         genericType_to_string_vector,
         genericType_to_void_pointer,
         genericType_type_name,
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
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__0" c_casadi__GenericType__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__0
  :: M.Map String GenericType -> IO GenericType
casadi__GenericType__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__0 :: M.Map String GenericType -> IO GenericType
genericType__0 = casadi__GenericType__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__1" c_casadi__GenericType__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr Function')) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__1
  :: Vector Function -> IO GenericType
casadi__GenericType__CONSTRUCTOR__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__1 :: Vector Function -> IO GenericType
genericType__1 = casadi__GenericType__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__2" c_casadi__GenericType__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__2
  :: Function -> IO GenericType
casadi__GenericType__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__2 :: Function -> IO GenericType
genericType__2 = casadi__GenericType__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__3" c_casadi__GenericType__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__3
  :: Vector String -> IO GenericType
casadi__GenericType__CONSTRUCTOR__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__3 :: Vector String -> IO GenericType
genericType__3 = casadi__GenericType__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__4" c_casadi__GenericType__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec CDouble))) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__4
  :: Vector (Vector Double) -> IO GenericType
casadi__GenericType__CONSTRUCTOR__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__4 :: Vector (Vector Double) -> IO GenericType
genericType__4 = casadi__GenericType__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__5" c_casadi__GenericType__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> Ptr (StdVec CDouble) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__5
  :: Vector Double -> IO GenericType
casadi__GenericType__CONSTRUCTOR__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__5 :: Vector Double -> IO GenericType
genericType__5 = casadi__GenericType__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__6" c_casadi__GenericType__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec CLLong))) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__6
  :: Vector (Vector Int) -> IO GenericType
casadi__GenericType__CONSTRUCTOR__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__6 :: Vector (Vector Int) -> IO GenericType
genericType__6 = casadi__GenericType__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__7" c_casadi__GenericType__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__7
  :: Vector Int -> IO GenericType
casadi__GenericType__CONSTRUCTOR__7 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__7 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__7 :: Vector Int -> IO GenericType
genericType__7 = casadi__GenericType__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__8" c_casadi__GenericType__CONSTRUCTOR__8
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__8
  :: Vector Int -> IO GenericType
casadi__GenericType__CONSTRUCTOR__8 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__8 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__8 :: Vector Int -> IO GenericType
genericType__8 = casadi__GenericType__CONSTRUCTOR__8


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__9" c_casadi__GenericType__CONSTRUCTOR__9
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__9
  :: Vector Bool -> IO GenericType
casadi__GenericType__CONSTRUCTOR__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__9 :: Vector Bool -> IO GenericType
genericType__9 = casadi__GenericType__CONSTRUCTOR__9


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__10" c_casadi__GenericType__CONSTRUCTOR__10
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__10
  :: String -> IO GenericType
casadi__GenericType__CONSTRUCTOR__10 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__10 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__10 :: String -> IO GenericType
genericType__10 = casadi__GenericType__CONSTRUCTOR__10


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__11" c_casadi__GenericType__CONSTRUCTOR__11
  :: Ptr (Ptr StdString) -> CDouble -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__11
  :: Double -> IO GenericType
casadi__GenericType__CONSTRUCTOR__11 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__11 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__11 :: Double -> IO GenericType
genericType__11 = casadi__GenericType__CONSTRUCTOR__11


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__12" c_casadi__GenericType__CONSTRUCTOR__12
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__12
  :: Int -> IO GenericType
casadi__GenericType__CONSTRUCTOR__12 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__12 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__12 :: Int -> IO GenericType
genericType__12 = casadi__GenericType__CONSTRUCTOR__12


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__13" c_casadi__GenericType__CONSTRUCTOR__13
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__13
  :: Int -> IO GenericType
casadi__GenericType__CONSTRUCTOR__13 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__13 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__13 :: Int -> IO GenericType
genericType__13 = casadi__GenericType__CONSTRUCTOR__13


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__14" c_casadi__GenericType__CONSTRUCTOR__14
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__14
  :: Bool -> IO GenericType
casadi__GenericType__CONSTRUCTOR__14 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__14 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType__14 :: Bool -> IO GenericType
genericType__14 = casadi__GenericType__CONSTRUCTOR__14


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__CONSTRUCTOR__15" c_casadi__GenericType__CONSTRUCTOR__15
  :: Ptr (Ptr StdString) -> IO (Ptr GenericType')

casadi__GenericType__CONSTRUCTOR__15
  :: IO GenericType
casadi__GenericType__CONSTRUCTOR__15  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__CONSTRUCTOR__15 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
genericType__15 :: IO GenericType
genericType__15 = casadi__GenericType__CONSTRUCTOR__15


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_bool" c_casadi__GenericType__as_bool
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__as_bool
  :: GenericType -> IO Bool
casadi__GenericType__as_bool x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_bool errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_bool :: GenericTypeClass a => a -> IO Bool
genericType_as_bool x = casadi__GenericType__as_bool (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_bool_vector" c_casadi__GenericType__as_bool_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CLLong))

casadi__GenericType__as_bool_vector
  :: GenericType -> IO (Vector Int)
casadi__GenericType__as_bool_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_bool_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_bool_vector :: GenericTypeClass a => a -> IO (Vector Int)
genericType_as_bool_vector x = casadi__GenericType__as_bool_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_dict" c_casadi__GenericType__as_dict
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__GenericType__as_dict
  :: GenericType -> IO (M.Map String GenericType)
casadi__GenericType__as_dict x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_dict errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_dict :: GenericTypeClass a => a -> IO (M.Map String GenericType)
genericType_as_dict x = casadi__GenericType__as_dict (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_double" c_casadi__GenericType__as_double
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CDouble

casadi__GenericType__as_double
  :: GenericType -> IO Double
casadi__GenericType__as_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_double :: GenericTypeClass a => a -> IO Double
genericType_as_double x = casadi__GenericType__as_double (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_double_vector" c_casadi__GenericType__as_double_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CDouble))

casadi__GenericType__as_double_vector
  :: GenericType -> IO (Vector Double)
casadi__GenericType__as_double_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_double_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_double_vector :: GenericTypeClass a => a -> IO (Vector Double)
genericType_as_double_vector x = casadi__GenericType__as_double_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_double_vector_vector" c_casadi__GenericType__as_double_vector_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr (StdVec CDouble))))

casadi__GenericType__as_double_vector_vector
  :: GenericType -> IO (Vector (Vector Double))
casadi__GenericType__as_double_vector_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_double_vector_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_double_vector_vector :: GenericTypeClass a => a -> IO (Vector (Vector Double))
genericType_as_double_vector_vector x = casadi__GenericType__as_double_vector_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_function" c_casadi__GenericType__as_function
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr Function')

casadi__GenericType__as_function
  :: GenericType -> IO Function
casadi__GenericType__as_function x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_function errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_function :: GenericTypeClass a => a -> IO Function
genericType_as_function x = casadi__GenericType__as_function (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_function_vector" c_casadi__GenericType__as_function_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr Function')))

casadi__GenericType__as_function_vector
  :: GenericType -> IO (Vector Function)
casadi__GenericType__as_function_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_function_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_function_vector :: GenericTypeClass a => a -> IO (Vector Function)
genericType_as_function_vector x = casadi__GenericType__as_function_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_int" c_casadi__GenericType__as_int
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CLLong

casadi__GenericType__as_int
  :: GenericType -> IO Int
casadi__GenericType__as_int x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_int errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_int :: GenericTypeClass a => a -> IO Int
genericType_as_int x = casadi__GenericType__as_int (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_int_vector" c_casadi__GenericType__as_int_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CLLong))

casadi__GenericType__as_int_vector
  :: GenericType -> IO (Vector Int)
casadi__GenericType__as_int_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_int_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_int_vector :: GenericTypeClass a => a -> IO (Vector Int)
genericType_as_int_vector x = casadi__GenericType__as_int_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_int_vector_vector" c_casadi__GenericType__as_int_vector_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr (StdVec CLLong))))

casadi__GenericType__as_int_vector_vector
  :: GenericType -> IO (Vector (Vector Int))
casadi__GenericType__as_int_vector_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_int_vector_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_int_vector_vector :: GenericTypeClass a => a -> IO (Vector (Vector Int))
genericType_as_int_vector_vector x = casadi__GenericType__as_int_vector_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_string" c_casadi__GenericType__as_string
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr StdString)

casadi__GenericType__as_string
  :: GenericType -> IO String
casadi__GenericType__as_string x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_string errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_string :: GenericTypeClass a => a -> IO String
genericType_as_string x = casadi__GenericType__as_string (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_string_vector" c_casadi__GenericType__as_string_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr StdString)))

casadi__GenericType__as_string_vector
  :: GenericType -> IO (Vector String)
casadi__GenericType__as_string_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_string_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_as_string_vector :: GenericTypeClass a => a -> IO (Vector String)
genericType_as_string_vector x = casadi__GenericType__as_string_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__as_void_pointer" c_casadi__GenericType__as_void_pointer
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO ()

casadi__GenericType__as_void_pointer
  :: GenericType -> IO ()
casadi__GenericType__as_void_pointer x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__as_void_pointer errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
genericType_as_void_pointer :: GenericTypeClass a => a -> IO ()
genericType_as_void_pointer x = casadi__GenericType__as_void_pointer (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__getType" c_casadi__GenericType__getType
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__getType
  :: GenericType -> IO TypeID
casadi__GenericType__getType x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__getType errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_getType :: GenericTypeClass a => a -> IO TypeID
genericType_getType x = casadi__GenericType__getType (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__get_description" c_casadi__GenericType__get_description
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr StdString)

casadi__GenericType__get_description
  :: GenericType -> IO String
casadi__GenericType__get_description x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__get_description errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_get_description :: GenericTypeClass a => a -> IO String
genericType_get_description x = casadi__GenericType__get_description (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_bool" c_casadi__GenericType__is_bool
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_bool
  :: GenericType -> IO Bool
casadi__GenericType__is_bool x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_bool errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_bool :: GenericTypeClass a => a -> IO Bool
genericType_is_bool x = casadi__GenericType__is_bool (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_bool_vector" c_casadi__GenericType__is_bool_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_bool_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_bool_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_bool_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_bool_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_bool_vector x = casadi__GenericType__is_bool_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_dict" c_casadi__GenericType__is_dict
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_dict
  :: GenericType -> IO Bool
casadi__GenericType__is_dict x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_dict errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_dict :: GenericTypeClass a => a -> IO Bool
genericType_is_dict x = casadi__GenericType__is_dict (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_double" c_casadi__GenericType__is_double
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_double
  :: GenericType -> IO Bool
casadi__GenericType__is_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_double :: GenericTypeClass a => a -> IO Bool
genericType_is_double x = casadi__GenericType__is_double (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_double_vector" c_casadi__GenericType__is_double_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_double_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_double_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_double_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_double_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_double_vector x = casadi__GenericType__is_double_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_double_vector_vector" c_casadi__GenericType__is_double_vector_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_double_vector_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_double_vector_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_double_vector_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_double_vector_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_double_vector_vector x = casadi__GenericType__is_double_vector_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_empty_vector" c_casadi__GenericType__is_empty_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_empty_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_empty_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_empty_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_empty_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_empty_vector x = casadi__GenericType__is_empty_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_function" c_casadi__GenericType__is_function
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_function
  :: GenericType -> IO Bool
casadi__GenericType__is_function x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_function errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_function :: GenericTypeClass a => a -> IO Bool
genericType_is_function x = casadi__GenericType__is_function (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_function_vector" c_casadi__GenericType__is_function_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_function_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_function_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_function_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_function_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_function_vector x = casadi__GenericType__is_function_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_int" c_casadi__GenericType__is_int
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_int
  :: GenericType -> IO Bool
casadi__GenericType__is_int x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_int errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_int :: GenericTypeClass a => a -> IO Bool
genericType_is_int x = casadi__GenericType__is_int (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_int_vector" c_casadi__GenericType__is_int_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_int_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_int_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_int_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_int_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_int_vector x = casadi__GenericType__is_int_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_int_vector_vector" c_casadi__GenericType__is_int_vector_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_int_vector_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_int_vector_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_int_vector_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_int_vector_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_int_vector_vector x = casadi__GenericType__is_int_vector_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_string" c_casadi__GenericType__is_string
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_string
  :: GenericType -> IO Bool
casadi__GenericType__is_string x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_string errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_string :: GenericTypeClass a => a -> IO Bool
genericType_is_string x = casadi__GenericType__is_string (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_string_vector" c_casadi__GenericType__is_string_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_string_vector
  :: GenericType -> IO Bool
casadi__GenericType__is_string_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_string_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_string_vector :: GenericTypeClass a => a -> IO Bool
genericType_is_string_vector x = casadi__GenericType__is_string_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__is_void_pointer" c_casadi__GenericType__is_void_pointer
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__is_void_pointer
  :: GenericType -> IO Bool
casadi__GenericType__is_void_pointer x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__is_void_pointer errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_is_void_pointer :: GenericTypeClass a => a -> IO Bool
genericType_is_void_pointer x = casadi__GenericType__is_void_pointer (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__operator__nequals" c_casadi__GenericType__operator__nequals
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> Ptr GenericType' -> IO CInt

casadi__GenericType__operator__nequals
  :: GenericType -> GenericType -> IO Bool
casadi__GenericType__operator__nequals x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__operator__nequals errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
genericType_operator__nequals :: GenericTypeClass a => a -> GenericType -> IO Bool
genericType_operator__nequals x = casadi__GenericType__operator__nequals (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__operator__equals" c_casadi__GenericType__operator__equals
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> Ptr GenericType' -> IO CInt

casadi__GenericType__operator__equals
  :: GenericType -> GenericType -> IO Bool
casadi__GenericType__operator__equals x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__operator__equals errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
genericType_operator__equals :: GenericTypeClass a => a -> GenericType -> IO Bool
genericType_operator__equals x = casadi__GenericType__operator__equals (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_bool" c_casadi__GenericType__to_bool
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CInt

casadi__GenericType__to_bool
  :: GenericType -> IO Bool
casadi__GenericType__to_bool x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_bool errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_bool :: GenericTypeClass a => a -> IO Bool
genericType_to_bool x = casadi__GenericType__to_bool (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_bool_vector" c_casadi__GenericType__to_bool_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CInt))

casadi__GenericType__to_bool_vector
  :: GenericType -> IO (Vector Bool)
casadi__GenericType__to_bool_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_bool_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_bool_vector :: GenericTypeClass a => a -> IO (Vector Bool)
genericType_to_bool_vector x = casadi__GenericType__to_bool_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_dict" c_casadi__GenericType__to_dict
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__GenericType__to_dict
  :: GenericType -> IO (M.Map String GenericType)
casadi__GenericType__to_dict x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_dict errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_dict :: GenericTypeClass a => a -> IO (M.Map String GenericType)
genericType_to_dict x = casadi__GenericType__to_dict (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_double" c_casadi__GenericType__to_double
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CDouble

casadi__GenericType__to_double
  :: GenericType -> IO Double
casadi__GenericType__to_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_double :: GenericTypeClass a => a -> IO Double
genericType_to_double x = casadi__GenericType__to_double (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_double_vector" c_casadi__GenericType__to_double_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CDouble))

casadi__GenericType__to_double_vector
  :: GenericType -> IO (Vector Double)
casadi__GenericType__to_double_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_double_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_double_vector :: GenericTypeClass a => a -> IO (Vector Double)
genericType_to_double_vector x = casadi__GenericType__to_double_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_double_vector_vector" c_casadi__GenericType__to_double_vector_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr (StdVec CDouble))))

casadi__GenericType__to_double_vector_vector
  :: GenericType -> IO (Vector (Vector Double))
casadi__GenericType__to_double_vector_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_double_vector_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_double_vector_vector :: GenericTypeClass a => a -> IO (Vector (Vector Double))
genericType_to_double_vector_vector x = casadi__GenericType__to_double_vector_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_function" c_casadi__GenericType__to_function
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr Function')

casadi__GenericType__to_function
  :: GenericType -> IO Function
casadi__GenericType__to_function x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_function errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_function :: GenericTypeClass a => a -> IO Function
genericType_to_function x = casadi__GenericType__to_function (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_function_vector" c_casadi__GenericType__to_function_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr Function')))

casadi__GenericType__to_function_vector
  :: GenericType -> IO (Vector Function)
casadi__GenericType__to_function_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_function_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_function_vector :: GenericTypeClass a => a -> IO (Vector Function)
genericType_to_function_vector x = casadi__GenericType__to_function_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_int" c_casadi__GenericType__to_int
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO CLLong

casadi__GenericType__to_int
  :: GenericType -> IO Int
casadi__GenericType__to_int x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_int errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_int :: GenericTypeClass a => a -> IO Int
genericType_to_int x = casadi__GenericType__to_int (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_int_type_vector" c_casadi__GenericType__to_int_type_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CInt))

casadi__GenericType__to_int_type_vector
  :: GenericType -> IO (Vector Int)
casadi__GenericType__to_int_type_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_int_type_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_int_type_vector :: GenericTypeClass a => a -> IO (Vector Int)
genericType_to_int_type_vector x = casadi__GenericType__to_int_type_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_int_vector" c_casadi__GenericType__to_int_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec CLLong))

casadi__GenericType__to_int_vector
  :: GenericType -> IO (Vector Int)
casadi__GenericType__to_int_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_int_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_int_vector :: GenericTypeClass a => a -> IO (Vector Int)
genericType_to_int_vector x = casadi__GenericType__to_int_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_int_vector_vector" c_casadi__GenericType__to_int_vector_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr (StdVec CLLong))))

casadi__GenericType__to_int_vector_vector
  :: GenericType -> IO (Vector (Vector Int))
casadi__GenericType__to_int_vector_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_int_vector_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_int_vector_vector :: GenericTypeClass a => a -> IO (Vector (Vector Int))
genericType_to_int_vector_vector x = casadi__GenericType__to_int_vector_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_string" c_casadi__GenericType__to_string
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr StdString)

casadi__GenericType__to_string
  :: GenericType -> IO String
casadi__GenericType__to_string x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_string errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_string :: GenericTypeClass a => a -> IO String
genericType_to_string x = casadi__GenericType__to_string (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_string_vector" c_casadi__GenericType__to_string_vector
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO (Ptr (StdVec (Ptr StdString)))

casadi__GenericType__to_string_vector
  :: GenericType -> IO (Vector String)
casadi__GenericType__to_string_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_string_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
genericType_to_string_vector :: GenericTypeClass a => a -> IO (Vector String)
genericType_to_string_vector x = casadi__GenericType__to_string_vector (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__to_void_pointer" c_casadi__GenericType__to_void_pointer
  :: Ptr (Ptr StdString) -> Ptr GenericType' -> IO ()

casadi__GenericType__to_void_pointer
  :: GenericType -> IO ()
casadi__GenericType__to_void_pointer x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__to_void_pointer errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
genericType_to_void_pointer :: GenericTypeClass a => a -> IO ()
genericType_to_void_pointer x = casadi__GenericType__to_void_pointer (castGenericType x)


-- direct wrapper
foreign import ccall unsafe "casadi__GenericType__type_name" c_casadi__GenericType__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__GenericType__type_name
  :: IO String
casadi__GenericType__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GenericType__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
genericType_type_name :: IO String
genericType_type_name = casadi__GenericType__type_name

