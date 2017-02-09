{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.MX
       (
         MX,
         MXClass(..),
         mx_T,
         mx__0,
         mx__1,
         mx__2,
         mx__3,
         mx__4,
         mx__5,
         mx__6,
         mx__7,
         mx___nonzero__,
         mx_attachAssert__0,
         mx_attachAssert__1,
         mx_binary,
         mx_colind,
         mx_dep__0,
         mx_dep__1,
         mx_dim,
         mx_enlarge__0,
         mx_enlarge__1,
         mx_erase__0,
         mx_erase__1,
         mx_erase__2,
         mx_erase__3,
         mx_eye,
         mx_getFunction__0,
         mx_getFunction__1,
         mx_getOutput__0,
         mx_getOutput__1,
         mx_get__0,
         mx_get__1,
         mx_get__2,
         mx_get__3,
         mx_get__4,
         mx_get__5,
         mx_get__6,
         mx_get_colind,
         mx_get_free,
         mx_get_input,
         mx_get_max_depth,
         mx_get_nz__0,
         mx_get_nz__1,
         mx_get_output,
         mx_get_row,
         mx_get_sparsity,
         mx_get_temp,
         mx_has_duplicates,
         mx_inf__0,
         mx_inf__1,
         mx_inf__2,
         mx_inf__3,
         mx_inf__4,
         mx_is_binary,
         mx_is_call,
         mx_is_column,
         mx_is_commutative,
         mx_is_constant,
         mx_is_dense,
         mx_is_empty__0,
         mx_is_empty__1,
         mx_is_identity,
         mx_is_minus_one,
         mx_is_multiplication,
         mx_is_norm,
         mx_is_one,
         mx_is_op,
         mx_is_output,
         mx_is_regular,
         mx_is_row,
         mx_is_scalar__0,
         mx_is_scalar__1,
         mx_is_square,
         mx_is_symbolic,
         mx_is_transpose,
         mx_is_tril,
         mx_is_triu,
         mx_is_unary,
         mx_is_valid_input,
         mx_is_vector,
         mx_is_zero,
         mx_join_primitives,
         mx_mapping,
         mx_monitor,
         mx_n_dep,
         mx_n_out,
         mx_n_primitives,
         mx_name,
         mx_nan__0,
         mx_nan__1,
         mx_nan__2,
         mx_nan__3,
         mx_nan__4,
         mx_nnz,
         mx_nnz_diag,
         mx_nnz_lower,
         mx_nnz_upper,
         mx_numFunctions,
         mx_numel,
         mx_ones__0,
         mx_ones__1,
         mx_ones__2,
         mx_ones__3,
         mx_ones__4,
         mx_op,
         mx_operator__minus,
         mx_operator_casadi__Matrix_double_,
         mx_operator_double,
         mx_primitives,
         mx_printme,
         mx_reset_input,
         mx_row,
         mx_set__0,
         mx_set__1,
         mx_set__2,
         mx_set__3,
         mx_set__4,
         mx_set__5,
         mx_set__6,
         mx_set_max_depth__0,
         mx_set_max_depth__1,
         mx_set_nz__0,
         mx_set_nz__1,
         mx_set_temp,
         mx_size1,
         mx_size2,
         mx_size__0,
         mx_size__1,
         mx_sparsity,
         mx_split_primitives,
         mx_sym__0,
         mx_sym__1,
         mx_sym__2,
         mx_sym__3,
         mx_sym__4,
         mx_sym__5,
         mx_sym__6,
         mx_sym__7,
         mx_sym__8,
         mx_type_name,
         mx_unary,
         mx_zeros__0,
         mx_zeros__1,
         mx_zeros__2,
         mx_zeros__3,
         mx_zeros__4,
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
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__0" c_casadi__MX__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__0
  :: DM -> IO MX
casadi__MX__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx__0 :: DM -> IO MX
mx__0 = casadi__MX__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__1" c_casadi__MX__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec CDouble) -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__1
  :: Vector Double -> IO MX
casadi__MX__CONSTRUCTOR__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx__1 :: Vector Double -> IO MX
mx__1 = casadi__MX__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__2" c_casadi__MX__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__2
  :: MX -> IO MX
casadi__MX__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx__2 :: MX -> IO MX
mx__2 = casadi__MX__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__3" c_casadi__MX__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> CDouble -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__3
  :: Double -> IO MX
casadi__MX__CONSTRUCTOR__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx__3 :: Double -> IO MX
mx__3 = casadi__MX__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__4" c_casadi__MX__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr MX' -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__4
  :: Sparsity -> MX -> IO MX
casadi__MX__CONSTRUCTOR__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx__4 :: Sparsity -> MX -> IO MX
mx__4 = casadi__MX__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__5" c_casadi__MX__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__5
  :: Sparsity -> IO MX
casadi__MX__CONSTRUCTOR__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx__5 :: Sparsity -> IO MX
mx__5 = casadi__MX__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__6" c_casadi__MX__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__6
  :: Int -> Int -> IO MX
casadi__MX__CONSTRUCTOR__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx__6 :: Int -> Int -> IO MX
mx__6 = casadi__MX__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__MX__CONSTRUCTOR__7" c_casadi__MX__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> IO (Ptr MX')

casadi__MX__CONSTRUCTOR__7
  :: IO MX
casadi__MX__CONSTRUCTOR__7  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__CONSTRUCTOR__7 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx__7 :: IO MX
mx__7 = casadi__MX__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__MX__T" c_casadi__MX__T
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi__MX__T
  :: MX -> IO MX
casadi__MX__T x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__T errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_T :: MXClass a => a -> IO MX
mx_T x = casadi__MX__T (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX____nonzero__" c_casadi__MX____nonzero__
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX____nonzero__
  :: MX -> IO Bool
casadi__MX____nonzero__ x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX____nonzero__ errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx___nonzero__ :: MXClass a => a -> IO Bool
mx___nonzero__ x = casadi__MX____nonzero__ (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__attachAssert__0" c_casadi__MX__attachAssert__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi__MX__attachAssert__0
  :: MX -> MX -> IO MX
casadi__MX__attachAssert__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__attachAssert__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_attachAssert__0 :: MXClass a => a -> MX -> IO MX
mx_attachAssert__0 x = casadi__MX__attachAssert__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__attachAssert__1" c_casadi__MX__attachAssert__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr StdString -> IO (Ptr MX')

casadi__MX__attachAssert__1
  :: MX -> MX -> String -> IO MX
casadi__MX__attachAssert__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__attachAssert__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
mx_attachAssert__1 :: MXClass a => a -> MX -> String -> IO MX
mx_attachAssert__1 x = casadi__MX__attachAssert__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__binary" c_casadi__MX__binary
  :: Ptr (Ptr StdString) -> CInt -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi__MX__binary
  :: Int -> MX -> MX -> IO MX
casadi__MX__binary x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__binary errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
mx_binary :: Int -> MX -> MX -> IO MX
mx_binary = casadi__MX__binary


-- direct wrapper
foreign import ccall unsafe "casadi__MX__colind" c_casadi__MX__colind
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO CInt

casadi__MX__colind
  :: MX -> Int -> IO Int
casadi__MX__colind x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__colind errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_colind :: MXClass a => a -> Int -> IO Int
mx_colind x = casadi__MX__colind (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__dep__0" c_casadi__MX__dep__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi__MX__dep__0
  :: MX -> IO MX
casadi__MX__dep__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__dep__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_dep__0 :: MXClass a => a -> IO MX
mx_dep__0 x = casadi__MX__dep__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__dep__1" c_casadi__MX__dep__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi__MX__dep__1
  :: MX -> Int -> IO MX
casadi__MX__dep__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__dep__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_dep__1 :: MXClass a => a -> Int -> IO MX
mx_dep__1 x = casadi__MX__dep__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__dim" c_casadi__MX__dim
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr StdString)

casadi__MX__dim
  :: MX -> IO String
casadi__MX__dim x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__dim errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_dim :: MXClass a => a -> IO String
mx_dim x = casadi__MX__dim (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__enlarge__0" c_casadi__MX__enlarge__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO ()

casadi__MX__enlarge__0
  :: MX -> Int -> Int -> Vector Int -> Vector Int -> IO ()
casadi__MX__enlarge__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__enlarge__0 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



-- classy wrapper
mx_enlarge__0 :: MXClass a => a -> Int -> Int -> Vector Int -> Vector Int -> IO ()
mx_enlarge__0 x = casadi__MX__enlarge__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__enlarge__1" c_casadi__MX__enlarge__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__MX__enlarge__1
  :: MX -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__MX__enlarge__1 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__enlarge__1 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ()



-- classy wrapper
mx_enlarge__1 :: MXClass a => a -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
mx_enlarge__1 x = casadi__MX__enlarge__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__erase__0" c_casadi__MX__erase__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> IO ()

casadi__MX__erase__0
  :: MX -> Vector Int -> IO ()
casadi__MX__erase__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__erase__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
mx_erase__0 :: MXClass a => a -> Vector Int -> IO ()
mx_erase__0 x = casadi__MX__erase__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__erase__1" c_casadi__MX__erase__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__MX__erase__1
  :: MX -> Vector Int -> Bool -> IO ()
casadi__MX__erase__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__erase__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
mx_erase__1 :: MXClass a => a -> Vector Int -> Bool -> IO ()
mx_erase__1 x = casadi__MX__erase__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__erase__2" c_casadi__MX__erase__2
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO ()

casadi__MX__erase__2
  :: MX -> Vector Int -> Vector Int -> IO ()
casadi__MX__erase__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__erase__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
mx_erase__2 :: MXClass a => a -> Vector Int -> Vector Int -> IO ()
mx_erase__2 x = casadi__MX__erase__2 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__erase__3" c_casadi__MX__erase__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__MX__erase__3
  :: MX -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__MX__erase__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__erase__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
mx_erase__3 :: MXClass a => a -> Vector Int -> Vector Int -> Bool -> IO ()
mx_erase__3 x = casadi__MX__erase__3 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__eye" c_casadi__MX__eye
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr MX')

casadi__MX__eye
  :: Int -> IO MX
casadi__MX__eye x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__eye errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_eye :: Int -> IO MX
mx_eye = casadi__MX__eye


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__0" c_casadi__MX__get__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__MX__get__0
  :: MX -> Bool -> IM -> IM -> IO (MX)
casadi__MX__get__0 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__0 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__0/c_casadi__MX__get__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
mx_get__0 :: MXClass a => a -> Bool -> IM -> IM -> IO (MX)
mx_get__0 x = casadi__MX__get__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__1" c_casadi__MX__get__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__MX__get__1
  :: MX -> Bool -> IM -> Slice -> IO (MX)
casadi__MX__get__1 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__1 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__1/c_casadi__MX__get__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
mx_get__1 :: MXClass a => a -> Bool -> IM -> Slice -> IO (MX)
mx_get__1 x = casadi__MX__get__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__2" c_casadi__MX__get__2
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__MX__get__2
  :: MX -> Bool -> Slice -> IM -> IO (MX)
casadi__MX__get__2 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__2 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__2/c_casadi__MX__get__2" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
mx_get__2 :: MXClass a => a -> Bool -> Slice -> IM -> IO (MX)
mx_get__2 x = casadi__MX__get__2 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__3" c_casadi__MX__get__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__MX__get__3
  :: MX -> Bool -> Slice -> Slice -> IO (MX)
casadi__MX__get__3 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__3 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__3/c_casadi__MX__get__3" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
mx_get__3 :: MXClass a => a -> Bool -> Slice -> Slice -> IO (MX)
mx_get__3 x = casadi__MX__get__3 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__4" c_casadi__MX__get__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr Sparsity' -> IO ()

casadi__MX__get__4
  :: MX -> Bool -> Sparsity -> IO (MX)
casadi__MX__get__4 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__4 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__4/c_casadi__MX__get__4" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
mx_get__4 :: MXClass a => a -> Bool -> Sparsity -> IO (MX)
mx_get__4 x = casadi__MX__get__4 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__5" c_casadi__MX__get__5
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr IM' -> IO ()

casadi__MX__get__5
  :: MX -> Bool -> IM -> IO (MX)
casadi__MX__get__5 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__5 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__5/c_casadi__MX__get__5" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
mx_get__5 :: MXClass a => a -> Bool -> IM -> IO (MX)
mx_get__5 x = casadi__MX__get__5 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get__6" c_casadi__MX__get__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr Slice' -> IO ()

casadi__MX__get__6
  :: MX -> Bool -> Slice -> IO (MX)
casadi__MX__get__6 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get__6 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get__6/c_casadi__MX__get__6" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
mx_get__6 :: MXClass a => a -> Bool -> Slice -> IO (MX)
mx_get__6 x = casadi__MX__get__6 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__getFunction__0" c_casadi__MX__getFunction__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr Function')

casadi__MX__getFunction__0
  :: MX -> IO Function
casadi__MX__getFunction__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__getFunction__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_getFunction__0 :: MXClass a => a -> IO Function
mx_getFunction__0 x = casadi__MX__getFunction__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__getFunction__1" c_casadi__MX__getFunction__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr Function')

casadi__MX__getFunction__1
  :: MX -> Int -> IO Function
casadi__MX__getFunction__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__getFunction__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_getFunction__1 :: MXClass a => a -> Int -> IO Function
mx_getFunction__1 x = casadi__MX__getFunction__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__getOutput__0" c_casadi__MX__getOutput__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi__MX__getOutput__0
  :: MX -> IO MX
casadi__MX__getOutput__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__getOutput__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_getOutput__0 :: MXClass a => a -> IO MX
mx_getOutput__0 x = casadi__MX__getOutput__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__getOutput__1" c_casadi__MX__getOutput__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO (Ptr MX')

casadi__MX__getOutput__1
  :: MX -> Int -> IO MX
casadi__MX__getOutput__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__getOutput__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_getOutput__1 :: MXClass a => a -> Int -> IO MX
mx_getOutput__1 x = casadi__MX__getOutput__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_colind" c_casadi__MX__get_colind
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec CInt))

casadi__MX__get_colind
  :: MX -> IO (Vector Int)
casadi__MX__get_colind x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_colind errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_colind :: MXClass a => a -> IO (Vector Int)
mx_get_colind x = casadi__MX__get_colind (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_free" c_casadi__MX__get_free
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr MX')))

casadi__MX__get_free
  :: Function -> IO (Vector MX)
casadi__MX__get_free x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_free errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_free :: Function -> IO (Vector MX)
mx_get_free = casadi__MX__get_free


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_input" c_casadi__MX__get_input
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr MX')))

casadi__MX__get_input
  :: Function -> IO (Vector MX)
casadi__MX__get_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_input :: Function -> IO (Vector MX)
mx_get_input = casadi__MX__get_input


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_max_depth" c_casadi__MX__get_max_depth
  :: Ptr (Ptr StdString) -> IO CInt

casadi__MX__get_max_depth
  :: IO Int
casadi__MX__get_max_depth  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_max_depth errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx_get_max_depth :: IO Int
mx_get_max_depth = casadi__MX__get_max_depth


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_nz__0" c_casadi__MX__get_nz__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr IM' -> IO ()

casadi__MX__get_nz__0
  :: MX -> Bool -> IM -> IO (MX)
casadi__MX__get_nz__0 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_nz__0 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get_nz__0/c_casadi__MX__get_nz__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
mx_get_nz__0 :: MXClass a => a -> Bool -> IM -> IO (MX)
mx_get_nz__0 x = casadi__MX__get_nz__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_nz__1" c_casadi__MX__get_nz__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (Ptr MX') -> CInt -> Ptr Slice' -> IO ()

casadi__MX__get_nz__1
  :: MX -> Bool -> Slice -> IO (MX)
casadi__MX__get_nz__1 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_nz__1 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__MX__get_nz__1/c_casadi__MX__get_nz__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
mx_get_nz__1 :: MXClass a => a -> Bool -> Slice -> IO (MX)
mx_get_nz__1 x = casadi__MX__get_nz__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_output" c_casadi__MX__get_output
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__get_output
  :: MX -> IO Int
casadi__MX__get_output x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_output errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_output :: MXClass a => a -> IO Int
mx_get_output x = casadi__MX__get_output (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_row" c_casadi__MX__get_row
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec CInt))

casadi__MX__get_row
  :: MX -> IO (Vector Int)
casadi__MX__get_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_row :: MXClass a => a -> IO (Vector Int)
mx_get_row x = casadi__MX__get_row (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_sparsity" c_casadi__MX__get_sparsity
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr Sparsity')

casadi__MX__get_sparsity
  :: MX -> IO Sparsity
casadi__MX__get_sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_sparsity :: MXClass a => a -> IO Sparsity
mx_get_sparsity x = casadi__MX__get_sparsity (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__get_temp" c_casadi__MX__get_temp
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__get_temp
  :: MX -> IO Int
casadi__MX__get_temp x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__get_temp errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_get_temp :: MXClass a => a -> IO Int
mx_get_temp x = casadi__MX__get_temp (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__has_duplicates" c_casadi__MX__has_duplicates
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__has_duplicates
  :: MX -> IO Bool
casadi__MX__has_duplicates x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__has_duplicates errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_has_duplicates :: MXClass a => a -> IO Bool
mx_has_duplicates x = casadi__MX__has_duplicates (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__inf__0" c_casadi__MX__inf__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi__MX__inf__0
  :: (Int, Int) -> IO MX
casadi__MX__inf__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__inf__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_inf__0 :: (Int, Int) -> IO MX
mx_inf__0 = casadi__MX__inf__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__inf__1" c_casadi__MX__inf__1
  :: Ptr (Ptr StdString) -> IO (Ptr MX')

casadi__MX__inf__1
  :: IO MX
casadi__MX__inf__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__inf__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx_inf__1 :: IO MX
mx_inf__1 = casadi__MX__inf__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__inf__2" c_casadi__MX__inf__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr MX')

casadi__MX__inf__2
  :: Int -> IO MX
casadi__MX__inf__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__inf__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_inf__2 :: Int -> IO MX
mx_inf__2 = casadi__MX__inf__2


-- direct wrapper
foreign import ccall unsafe "casadi__MX__inf__3" c_casadi__MX__inf__3
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr MX')

casadi__MX__inf__3
  :: Int -> Int -> IO MX
casadi__MX__inf__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__inf__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_inf__3 :: Int -> Int -> IO MX
mx_inf__3 = casadi__MX__inf__3


-- direct wrapper
foreign import ccall unsafe "casadi__MX__inf__4" c_casadi__MX__inf__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr MX')

casadi__MX__inf__4
  :: Sparsity -> IO MX
casadi__MX__inf__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__inf__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_inf__4 :: Sparsity -> IO MX
mx_inf__4 = casadi__MX__inf__4


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_binary" c_casadi__MX__is_binary
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_binary
  :: MX -> IO Bool
casadi__MX__is_binary x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_binary errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_binary :: MXClass a => a -> IO Bool
mx_is_binary x = casadi__MX__is_binary (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_call" c_casadi__MX__is_call
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_call
  :: MX -> IO Bool
casadi__MX__is_call x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_call errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_call :: MXClass a => a -> IO Bool
mx_is_call x = casadi__MX__is_call (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_column" c_casadi__MX__is_column
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_column
  :: MX -> IO Bool
casadi__MX__is_column x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_column errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_column :: MXClass a => a -> IO Bool
mx_is_column x = casadi__MX__is_column (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_commutative" c_casadi__MX__is_commutative
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_commutative
  :: MX -> IO Bool
casadi__MX__is_commutative x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_commutative errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_commutative :: MXClass a => a -> IO Bool
mx_is_commutative x = casadi__MX__is_commutative (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_constant" c_casadi__MX__is_constant
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_constant
  :: MX -> IO Bool
casadi__MX__is_constant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_constant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_constant :: MXClass a => a -> IO Bool
mx_is_constant x = casadi__MX__is_constant (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_dense" c_casadi__MX__is_dense
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_dense
  :: MX -> IO Bool
casadi__MX__is_dense x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_dense errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_dense :: MXClass a => a -> IO Bool
mx_is_dense x = casadi__MX__is_dense (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_empty__0" c_casadi__MX__is_empty__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_empty__0
  :: MX -> IO Bool
casadi__MX__is_empty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_empty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_empty__0 :: MXClass a => a -> IO Bool
mx_is_empty__0 x = casadi__MX__is_empty__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_empty__1" c_casadi__MX__is_empty__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO CInt

casadi__MX__is_empty__1
  :: MX -> Bool -> IO Bool
casadi__MX__is_empty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_empty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_is_empty__1 :: MXClass a => a -> Bool -> IO Bool
mx_is_empty__1 x = casadi__MX__is_empty__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_identity" c_casadi__MX__is_identity
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_identity
  :: MX -> IO Bool
casadi__MX__is_identity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_identity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_identity :: MXClass a => a -> IO Bool
mx_is_identity x = casadi__MX__is_identity (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_minus_one" c_casadi__MX__is_minus_one
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_minus_one
  :: MX -> IO Bool
casadi__MX__is_minus_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_minus_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_minus_one :: MXClass a => a -> IO Bool
mx_is_minus_one x = casadi__MX__is_minus_one (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_multiplication" c_casadi__MX__is_multiplication
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_multiplication
  :: MX -> IO Bool
casadi__MX__is_multiplication x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_multiplication errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_multiplication :: MXClass a => a -> IO Bool
mx_is_multiplication x = casadi__MX__is_multiplication (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_norm" c_casadi__MX__is_norm
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_norm
  :: MX -> IO Bool
casadi__MX__is_norm x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_norm errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_norm :: MXClass a => a -> IO Bool
mx_is_norm x = casadi__MX__is_norm (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_one" c_casadi__MX__is_one
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_one
  :: MX -> IO Bool
casadi__MX__is_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_one :: MXClass a => a -> IO Bool
mx_is_one x = casadi__MX__is_one (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_op" c_casadi__MX__is_op
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO CInt

casadi__MX__is_op
  :: MX -> Int -> IO Bool
casadi__MX__is_op x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_op errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_is_op :: MXClass a => a -> Int -> IO Bool
mx_is_op x = casadi__MX__is_op (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_output" c_casadi__MX__is_output
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_output
  :: MX -> IO Bool
casadi__MX__is_output x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_output errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_output :: MXClass a => a -> IO Bool
mx_is_output x = casadi__MX__is_output (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_regular" c_casadi__MX__is_regular
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_regular
  :: MX -> IO Bool
casadi__MX__is_regular x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_regular errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_regular :: MXClass a => a -> IO Bool
mx_is_regular x = casadi__MX__is_regular (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_row" c_casadi__MX__is_row
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_row
  :: MX -> IO Bool
casadi__MX__is_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_row :: MXClass a => a -> IO Bool
mx_is_row x = casadi__MX__is_row (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_scalar__0" c_casadi__MX__is_scalar__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_scalar__0
  :: MX -> IO Bool
casadi__MX__is_scalar__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_scalar__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_scalar__0 :: MXClass a => a -> IO Bool
mx_is_scalar__0 x = casadi__MX__is_scalar__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_scalar__1" c_casadi__MX__is_scalar__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO CInt

casadi__MX__is_scalar__1
  :: MX -> Bool -> IO Bool
casadi__MX__is_scalar__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_scalar__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_is_scalar__1 :: MXClass a => a -> Bool -> IO Bool
mx_is_scalar__1 x = casadi__MX__is_scalar__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_square" c_casadi__MX__is_square
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_square
  :: MX -> IO Bool
casadi__MX__is_square x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_square errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_square :: MXClass a => a -> IO Bool
mx_is_square x = casadi__MX__is_square (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_symbolic" c_casadi__MX__is_symbolic
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_symbolic
  :: MX -> IO Bool
casadi__MX__is_symbolic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_symbolic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_symbolic :: MXClass a => a -> IO Bool
mx_is_symbolic x = casadi__MX__is_symbolic (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_transpose" c_casadi__MX__is_transpose
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_transpose
  :: MX -> IO Bool
casadi__MX__is_transpose x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_transpose errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_transpose :: MXClass a => a -> IO Bool
mx_is_transpose x = casadi__MX__is_transpose (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_tril" c_casadi__MX__is_tril
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_tril
  :: MX -> IO Bool
casadi__MX__is_tril x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_tril errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_tril :: MXClass a => a -> IO Bool
mx_is_tril x = casadi__MX__is_tril (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_triu" c_casadi__MX__is_triu
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_triu
  :: MX -> IO Bool
casadi__MX__is_triu x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_triu errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_triu :: MXClass a => a -> IO Bool
mx_is_triu x = casadi__MX__is_triu (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_unary" c_casadi__MX__is_unary
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_unary
  :: MX -> IO Bool
casadi__MX__is_unary x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_unary errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_unary :: MXClass a => a -> IO Bool
mx_is_unary x = casadi__MX__is_unary (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_valid_input" c_casadi__MX__is_valid_input
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_valid_input
  :: MX -> IO Bool
casadi__MX__is_valid_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_valid_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_valid_input :: MXClass a => a -> IO Bool
mx_is_valid_input x = casadi__MX__is_valid_input (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_vector" c_casadi__MX__is_vector
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_vector
  :: MX -> IO Bool
casadi__MX__is_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_vector :: MXClass a => a -> IO Bool
mx_is_vector x = casadi__MX__is_vector (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__is_zero" c_casadi__MX__is_zero
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__is_zero
  :: MX -> IO Bool
casadi__MX__is_zero x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__is_zero errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_is_zero :: MXClass a => a -> IO Bool
mx_is_zero x = casadi__MX__is_zero (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__join_primitives" c_casadi__MX__join_primitives
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr MX')

casadi__MX__join_primitives
  :: MX -> Vector MX -> IO MX
casadi__MX__join_primitives x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__join_primitives errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_join_primitives :: MXClass a => a -> Vector MX -> IO MX
mx_join_primitives x = casadi__MX__join_primitives (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__mapping" c_casadi__MX__mapping
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr IM')

casadi__MX__mapping
  :: MX -> IO IM
casadi__MX__mapping x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__mapping errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_mapping :: MXClass a => a -> IO IM
mx_mapping x = casadi__MX__mapping (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__monitor" c_casadi__MX__monitor
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr StdString -> IO (Ptr MX')

casadi__MX__monitor
  :: MX -> String -> IO MX
casadi__MX__monitor x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__monitor errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_monitor :: MXClass a => a -> String -> IO MX
mx_monitor x = casadi__MX__monitor (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__n_dep" c_casadi__MX__n_dep
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__n_dep
  :: MX -> IO Int
casadi__MX__n_dep x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__n_dep errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_n_dep :: MXClass a => a -> IO Int
mx_n_dep x = casadi__MX__n_dep (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__n_out" c_casadi__MX__n_out
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__n_out
  :: MX -> IO Int
casadi__MX__n_out x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__n_out errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_n_out :: MXClass a => a -> IO Int
mx_n_out x = casadi__MX__n_out (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__n_primitives" c_casadi__MX__n_primitives
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__n_primitives
  :: MX -> IO Int
casadi__MX__n_primitives x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__n_primitives errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_n_primitives :: MXClass a => a -> IO Int
mx_n_primitives x = casadi__MX__n_primitives (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__name" c_casadi__MX__name
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr StdString)

casadi__MX__name
  :: MX -> IO String
casadi__MX__name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_name :: MXClass a => a -> IO String
mx_name x = casadi__MX__name (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nan__0" c_casadi__MX__nan__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi__MX__nan__0
  :: (Int, Int) -> IO MX
casadi__MX__nan__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nan__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nan__0 :: (Int, Int) -> IO MX
mx_nan__0 = casadi__MX__nan__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nan__1" c_casadi__MX__nan__1
  :: Ptr (Ptr StdString) -> IO (Ptr MX')

casadi__MX__nan__1
  :: IO MX
casadi__MX__nan__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nan__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx_nan__1 :: IO MX
mx_nan__1 = casadi__MX__nan__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nan__2" c_casadi__MX__nan__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr MX')

casadi__MX__nan__2
  :: Int -> IO MX
casadi__MX__nan__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nan__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nan__2 :: Int -> IO MX
mx_nan__2 = casadi__MX__nan__2


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nan__3" c_casadi__MX__nan__3
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr MX')

casadi__MX__nan__3
  :: Int -> Int -> IO MX
casadi__MX__nan__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nan__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_nan__3 :: Int -> Int -> IO MX
mx_nan__3 = casadi__MX__nan__3


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nan__4" c_casadi__MX__nan__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr MX')

casadi__MX__nan__4
  :: Sparsity -> IO MX
casadi__MX__nan__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nan__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nan__4 :: Sparsity -> IO MX
mx_nan__4 = casadi__MX__nan__4


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nnz" c_casadi__MX__nnz
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__nnz
  :: MX -> IO Int
casadi__MX__nnz x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nnz errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nnz :: MXClass a => a -> IO Int
mx_nnz x = casadi__MX__nnz (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nnz_diag" c_casadi__MX__nnz_diag
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__nnz_diag
  :: MX -> IO Int
casadi__MX__nnz_diag x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nnz_diag errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nnz_diag :: MXClass a => a -> IO Int
mx_nnz_diag x = casadi__MX__nnz_diag (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nnz_lower" c_casadi__MX__nnz_lower
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__nnz_lower
  :: MX -> IO Int
casadi__MX__nnz_lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nnz_lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nnz_lower :: MXClass a => a -> IO Int
mx_nnz_lower x = casadi__MX__nnz_lower (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__nnz_upper" c_casadi__MX__nnz_upper
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__nnz_upper
  :: MX -> IO Int
casadi__MX__nnz_upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__nnz_upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_nnz_upper :: MXClass a => a -> IO Int
mx_nnz_upper x = casadi__MX__nnz_upper (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__numFunctions" c_casadi__MX__numFunctions
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__numFunctions
  :: MX -> IO Int
casadi__MX__numFunctions x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__numFunctions errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_numFunctions :: MXClass a => a -> IO Int
mx_numFunctions x = casadi__MX__numFunctions (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__numel" c_casadi__MX__numel
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__numel
  :: MX -> IO Int
casadi__MX__numel x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__numel errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_numel :: MXClass a => a -> IO Int
mx_numel x = casadi__MX__numel (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__ones__0" c_casadi__MX__ones__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi__MX__ones__0
  :: (Int, Int) -> IO MX
casadi__MX__ones__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__ones__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_ones__0 :: (Int, Int) -> IO MX
mx_ones__0 = casadi__MX__ones__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__ones__1" c_casadi__MX__ones__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr MX')

casadi__MX__ones__1
  :: Sparsity -> IO MX
casadi__MX__ones__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__ones__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_ones__1 :: Sparsity -> IO MX
mx_ones__1 = casadi__MX__ones__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__ones__2" c_casadi__MX__ones__2
  :: Ptr (Ptr StdString) -> IO (Ptr MX')

casadi__MX__ones__2
  :: IO MX
casadi__MX__ones__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__ones__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx_ones__2 :: IO MX
mx_ones__2 = casadi__MX__ones__2


-- direct wrapper
foreign import ccall unsafe "casadi__MX__ones__3" c_casadi__MX__ones__3
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr MX')

casadi__MX__ones__3
  :: Int -> IO MX
casadi__MX__ones__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__ones__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_ones__3 :: Int -> IO MX
mx_ones__3 = casadi__MX__ones__3


-- direct wrapper
foreign import ccall unsafe "casadi__MX__ones__4" c_casadi__MX__ones__4
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr MX')

casadi__MX__ones__4
  :: Int -> Int -> IO MX
casadi__MX__ones__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__ones__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_ones__4 :: Int -> Int -> IO MX
mx_ones__4 = casadi__MX__ones__4


-- direct wrapper
foreign import ccall unsafe "casadi__MX__op" c_casadi__MX__op
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__op
  :: MX -> IO Int
casadi__MX__op x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__op errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_op :: MXClass a => a -> IO Int
mx_op x = casadi__MX__op (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__operator__minus" c_casadi__MX__operator__minus
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr MX')

casadi__MX__operator__minus
  :: MX -> IO MX
casadi__MX__operator__minus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__operator__minus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_operator__minus :: MXClass a => a -> IO MX
mx_operator__minus x = casadi__MX__operator__minus (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__operator_casadi__Matrix_double_" c_casadi__MX__operator_casadi__Matrix_double_
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr DM')

casadi__MX__operator_casadi__Matrix_double_
  :: MX -> IO DM
casadi__MX__operator_casadi__Matrix_double_ x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__operator_casadi__Matrix_double_ errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_operator_casadi__Matrix_double_ :: MXClass a => a -> IO DM
mx_operator_casadi__Matrix_double_ x = casadi__MX__operator_casadi__Matrix_double_ (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__operator_double" c_casadi__MX__operator_double
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CDouble

casadi__MX__operator_double
  :: MX -> IO Double
casadi__MX__operator_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__operator_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_operator_double :: MXClass a => a -> IO Double
mx_operator_double x = casadi__MX__operator_double (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__primitives" c_casadi__MX__primitives
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi__MX__primitives
  :: MX -> IO (Vector MX)
casadi__MX__primitives x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__primitives errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_primitives :: MXClass a => a -> IO (Vector MX)
mx_primitives x = casadi__MX__primitives (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__printme" c_casadi__MX__printme
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi__MX__printme
  :: MX -> MX -> IO MX
casadi__MX__printme x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__printme errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_printme :: MXClass a => a -> MX -> IO MX
mx_printme x = casadi__MX__printme (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__reset_input" c_casadi__MX__reset_input
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO ()

casadi__MX__reset_input
  :: MX -> IO ()
casadi__MX__reset_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__reset_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
mx_reset_input :: MXClass a => a -> IO ()
mx_reset_input x = casadi__MX__reset_input (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__row" c_casadi__MX__row
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO CInt

casadi__MX__row
  :: MX -> Int -> IO Int
casadi__MX__row x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__row errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_row :: MXClass a => a -> Int -> IO Int
mx_row x = casadi__MX__row (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__0" c_casadi__MX__set__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__MX__set__0
  :: MX -> MX -> Bool -> IM -> IM -> IO ()
casadi__MX__set__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__0 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



-- classy wrapper
mx_set__0 :: MXClass a => a -> MX -> Bool -> IM -> IM -> IO ()
mx_set__0 x = casadi__MX__set__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__1" c_casadi__MX__set__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__MX__set__1
  :: MX -> MX -> Bool -> IM -> Slice -> IO ()
casadi__MX__set__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__1 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



-- classy wrapper
mx_set__1 :: MXClass a => a -> MX -> Bool -> IM -> Slice -> IO ()
mx_set__1 x = casadi__MX__set__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__2" c_casadi__MX__set__2
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__MX__set__2
  :: MX -> MX -> Bool -> Slice -> IM -> IO ()
casadi__MX__set__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__2 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



-- classy wrapper
mx_set__2 :: MXClass a => a -> MX -> Bool -> Slice -> IM -> IO ()
mx_set__2 x = casadi__MX__set__2 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__3" c_casadi__MX__set__3
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__MX__set__3
  :: MX -> MX -> Bool -> Slice -> Slice -> IO ()
casadi__MX__set__3 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__3 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ()



-- classy wrapper
mx_set__3 :: MXClass a => a -> MX -> Bool -> Slice -> Slice -> IO ()
mx_set__3 x = casadi__MX__set__3 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__4" c_casadi__MX__set__4
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr Sparsity' -> IO ()

casadi__MX__set__4
  :: MX -> MX -> Bool -> Sparsity -> IO ()
casadi__MX__set__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
mx_set__4 :: MXClass a => a -> MX -> Bool -> Sparsity -> IO ()
mx_set__4 x = casadi__MX__set__4 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__5" c_casadi__MX__set__5
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr IM' -> IO ()

casadi__MX__set__5
  :: MX -> MX -> Bool -> IM -> IO ()
casadi__MX__set__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
mx_set__5 :: MXClass a => a -> MX -> Bool -> IM -> IO ()
mx_set__5 x = casadi__MX__set__5 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set__6" c_casadi__MX__set__6
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr Slice' -> IO ()

casadi__MX__set__6
  :: MX -> MX -> Bool -> Slice -> IO ()
casadi__MX__set__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
mx_set__6 :: MXClass a => a -> MX -> Bool -> Slice -> IO ()
mx_set__6 x = casadi__MX__set__6 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set_max_depth__0" c_casadi__MX__set_max_depth__0
  :: Ptr (Ptr StdString) -> IO ()

casadi__MX__set_max_depth__0
  :: IO ()
casadi__MX__set_max_depth__0  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set_max_depth__0 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ()



-- classy wrapper
mx_set_max_depth__0 :: IO ()
mx_set_max_depth__0 = casadi__MX__set_max_depth__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set_max_depth__1" c_casadi__MX__set_max_depth__1
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__MX__set_max_depth__1
  :: Int -> IO ()
casadi__MX__set_max_depth__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set_max_depth__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
mx_set_max_depth__1 :: Int -> IO ()
mx_set_max_depth__1 = casadi__MX__set_max_depth__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set_nz__0" c_casadi__MX__set_nz__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr IM' -> IO ()

casadi__MX__set_nz__0
  :: MX -> MX -> Bool -> IM -> IO ()
casadi__MX__set_nz__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set_nz__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
mx_set_nz__0 :: MXClass a => a -> MX -> Bool -> IM -> IO ()
mx_set_nz__0 x = casadi__MX__set_nz__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set_nz__1" c_casadi__MX__set_nz__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> CInt -> Ptr Slice' -> IO ()

casadi__MX__set_nz__1
  :: MX -> MX -> Bool -> Slice -> IO ()
casadi__MX__set_nz__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set_nz__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
mx_set_nz__1 :: MXClass a => a -> MX -> Bool -> Slice -> IO ()
mx_set_nz__1 x = casadi__MX__set_nz__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__set_temp" c_casadi__MX__set_temp
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO ()

casadi__MX__set_temp
  :: MX -> Int -> IO ()
casadi__MX__set_temp x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__set_temp errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
mx_set_temp :: MXClass a => a -> Int -> IO ()
mx_set_temp x = casadi__MX__set_temp (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__size__0" c_casadi__MX__size__0
  :: Ptr (Ptr StdString) -> Ptr MX' -> CInt -> IO CInt

casadi__MX__size__0
  :: MX -> Int -> IO Int
casadi__MX__size__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__size__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_size__0 :: MXClass a => a -> Int -> IO Int
mx_size__0 x = casadi__MX__size__0 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__size__1" c_casadi__MX__size__1
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr (StdPair CInt CInt))

casadi__MX__size__1
  :: MX -> IO (Int, Int)
casadi__MX__size__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__size__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_size__1 :: MXClass a => a -> IO (Int, Int)
mx_size__1 x = casadi__MX__size__1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__size1" c_casadi__MX__size1
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__size1
  :: MX -> IO Int
casadi__MX__size1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__size1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_size1 :: MXClass a => a -> IO Int
mx_size1 x = casadi__MX__size1 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__size2" c_casadi__MX__size2
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO CInt

casadi__MX__size2
  :: MX -> IO Int
casadi__MX__size2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__size2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_size2 :: MXClass a => a -> IO Int
mx_size2 x = casadi__MX__size2 (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sparsity" c_casadi__MX__sparsity
  :: Ptr (Ptr StdString) -> Ptr MX' -> IO (Ptr Sparsity')

casadi__MX__sparsity
  :: MX -> IO Sparsity
casadi__MX__sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_sparsity :: MXClass a => a -> IO Sparsity
mx_sparsity x = casadi__MX__sparsity (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__split_primitives" c_casadi__MX__split_primitives
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi__MX__split_primitives
  :: MX -> MX -> IO (Vector MX)
casadi__MX__split_primitives x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__split_primitives errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_split_primitives :: MXClass a => a -> MX -> IO (Vector MX)
mx_split_primitives x = casadi__MX__split_primitives (castMX x)


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__0" c_casadi__MX__sym__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi__MX__sym__0
  :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector MX))
casadi__MX__sym__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__0 errStrPtrP x0' x1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return ret



-- classy wrapper
mx_sym__0 :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector MX))
mx_sym__0 = casadi__MX__sym__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__1" c_casadi__MX__sym__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr MX')))))

casadi__MX__sym__1
  :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector MX))
casadi__MX__sym__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
mx_sym__1 :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector MX))
mx_sym__1 = casadi__MX__sym__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__2" c_casadi__MX__sym__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi__MX__sym__2
  :: String -> Int -> Int -> Int -> IO (Vector MX)
casadi__MX__sym__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
mx_sym__2 :: String -> Int -> Int -> Int -> IO (Vector MX)
mx_sym__2 = casadi__MX__sym__2


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__3" c_casadi__MX__sym__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi__MX__sym__3
  :: String -> Sparsity -> Int -> IO (Vector MX)
casadi__MX__sym__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
mx_sym__3 :: String -> Sparsity -> Int -> IO (Vector MX)
mx_sym__3 = casadi__MX__sym__3


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__4" c_casadi__MX__sym__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr MX')

casadi__MX__sym__4
  :: String -> Sparsity -> IO MX
casadi__MX__sym__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_sym__4 :: String -> Sparsity -> IO MX
mx_sym__4 = casadi__MX__sym__4


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__5" c_casadi__MX__sym__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi__MX__sym__5
  :: String -> (Int, Int) -> IO MX
casadi__MX__sym__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_sym__5 :: String -> (Int, Int) -> IO MX
mx_sym__5 = casadi__MX__sym__5


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__6" c_casadi__MX__sym__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr MX')

casadi__MX__sym__6
  :: String -> IO MX
casadi__MX__sym__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_sym__6 :: String -> IO MX
mx_sym__6 = casadi__MX__sym__6


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__7" c_casadi__MX__sym__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> IO (Ptr MX')

casadi__MX__sym__7
  :: String -> Int -> IO MX
casadi__MX__sym__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_sym__7 :: String -> Int -> IO MX
mx_sym__7 = casadi__MX__sym__7


-- direct wrapper
foreign import ccall unsafe "casadi__MX__sym__8" c_casadi__MX__sym__8
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> IO (Ptr MX')

casadi__MX__sym__8
  :: String -> Int -> Int -> IO MX
casadi__MX__sym__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__sym__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
mx_sym__8 :: String -> Int -> Int -> IO MX
mx_sym__8 = casadi__MX__sym__8


-- direct wrapper
foreign import ccall unsafe "casadi__MX__type_name" c_casadi__MX__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__MX__type_name
  :: IO String
casadi__MX__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx_type_name :: IO String
mx_type_name = casadi__MX__type_name


-- direct wrapper
foreign import ccall unsafe "casadi__MX__unary" c_casadi__MX__unary
  :: Ptr (Ptr StdString) -> CInt -> Ptr MX' -> IO (Ptr MX')

casadi__MX__unary
  :: Int -> MX -> IO MX
casadi__MX__unary x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__unary errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_unary :: Int -> MX -> IO MX
mx_unary = casadi__MX__unary


-- direct wrapper
foreign import ccall unsafe "casadi__MX__zeros__0" c_casadi__MX__zeros__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr MX')

casadi__MX__zeros__0
  :: (Int, Int) -> IO MX
casadi__MX__zeros__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__zeros__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_zeros__0 :: (Int, Int) -> IO MX
mx_zeros__0 = casadi__MX__zeros__0


-- direct wrapper
foreign import ccall unsafe "casadi__MX__zeros__1" c_casadi__MX__zeros__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr MX')

casadi__MX__zeros__1
  :: Sparsity -> IO MX
casadi__MX__zeros__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__zeros__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_zeros__1 :: Sparsity -> IO MX
mx_zeros__1 = casadi__MX__zeros__1


-- direct wrapper
foreign import ccall unsafe "casadi__MX__zeros__2" c_casadi__MX__zeros__2
  :: Ptr (Ptr StdString) -> IO (Ptr MX')

casadi__MX__zeros__2
  :: IO MX
casadi__MX__zeros__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__zeros__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
mx_zeros__2 :: IO MX
mx_zeros__2 = casadi__MX__zeros__2


-- direct wrapper
foreign import ccall unsafe "casadi__MX__zeros__3" c_casadi__MX__zeros__3
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr MX')

casadi__MX__zeros__3
  :: Int -> IO MX
casadi__MX__zeros__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__zeros__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
mx_zeros__3 :: Int -> IO MX
mx_zeros__3 = casadi__MX__zeros__3


-- direct wrapper
foreign import ccall unsafe "casadi__MX__zeros__4" c_casadi__MX__zeros__4
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr MX')

casadi__MX__zeros__4
  :: Int -> Int -> IO MX
casadi__MX__zeros__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__MX__zeros__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
mx_zeros__4 :: Int -> Int -> IO MX
mx_zeros__4 = casadi__MX__zeros__4

