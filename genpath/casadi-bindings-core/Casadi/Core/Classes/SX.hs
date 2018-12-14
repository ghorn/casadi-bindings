{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.SX
       (
         SX,
         SXClass(..),
         sx_T,
         sx__0,
         sx__1,
         sx__10,
         sx__2,
         sx__3,
         sx__4,
         sx__5,
         sx__6,
         sx__7,
         sx__8,
         sx__9,
         sx___nonzero__,
         sx_binary,
         sx_clear,
         sx_colind,
         sx_columns,
         sx_dep__0,
         sx_dep__1,
         sx_dim__0,
         sx_dim__1,
         sx_element_hash,
         sx_enlarge__0,
         sx_enlarge__1,
         sx_erase__0,
         sx_erase__1,
         sx_erase__2,
         sx_erase__3,
         sx_export_code,
         sx_eye,
         sx_from_info,
         sx_get__0,
         sx_get__1,
         sx_get__2,
         sx_get__3,
         sx_get__4,
         sx_get__5,
         sx_get__6,
         sx_get_colind,
         sx_get_elements,
         sx_get_free,
         sx_get_input,
         sx_get_max_depth,
         sx_get_nonzeros,
         sx_get_nz__0,
         sx_get_nz__1,
         sx_get_row,
         sx_get_sparsity,
         sx_get_str__0,
         sx_get_str__1,
         sx_has_duplicates,
         sx_has_nz,
         sx_has_zeros,
         sx_inf__0,
         sx_inf__1,
         sx_inf__2,
         sx_inf__3,
         sx_inf__4,
         sx_info,
         sx_is_column,
         sx_is_commutative,
         sx_is_constant,
         sx_is_dense,
         sx_is_empty__0,
         sx_is_empty__1,
         sx_is_eye,
         sx_is_integer,
         sx_is_leaf,
         sx_is_minus_one,
         sx_is_one,
         sx_is_op,
         sx_is_regular,
         sx_is_row,
         sx_is_scalar__0,
         sx_is_scalar__1,
         sx_is_smooth,
         sx_is_square,
         sx_is_symbolic,
         sx_is_tril,
         sx_is_triu,
         sx_is_valid_input,
         sx_is_vector,
         sx_is_zero,
         sx_matrix_matrix,
         sx_matrix_scalar,
         sx_n_dep,
         sx_name,
         sx_nan__0,
         sx_nan__1,
         sx_nan__2,
         sx_nan__3,
         sx_nan__4,
         sx_nnz,
         sx_nnz_diag,
         sx_nnz_lower,
         sx_nnz_upper,
         sx_numel,
         sx_ones__0,
         sx_ones__1,
         sx_ones__2,
         sx_ones__3,
         sx_ones__4,
         sx_op,
         sx_operator__minus,
         sx_operator__plus,
         sx_operator_casadi_int,
         sx_operator_double,
         sx_print_split,
         sx_printme,
         sx_rand__0,
         sx_rand__1,
         sx_rand__2,
         sx_rand__3,
         sx_rand__4,
         sx_remove,
         sx_reserve__0,
         sx_reserve__1,
         sx_reset_input,
         sx_resize,
         sx_rng,
         sx_row,
         sx_rows,
         sx_scalar_matrix,
         sx_set__0,
         sx_set__1,
         sx_set__2,
         sx_set__3,
         sx_set__4,
         sx_set__5,
         sx_set__6,
         sx_set_max_depth__0,
         sx_set_max_depth__1,
         sx_set_nz__0,
         sx_set_nz__1,
         sx_set_precision,
         sx_set_scientific,
         sx_set_width,
         sx_size1,
         sx_size2,
         sx_size__0,
         sx_size__1,
         sx_sparsity,
         sx_sym__0,
         sx_sym__1,
         sx_sym__2,
         sx_sym__3,
         sx_sym__4,
         sx_sym__5,
         sx_sym__6,
         sx_sym__7,
         sx_sym__8,
         sx_to_file__0,
         sx_to_file__1,
         sx_triplet__0,
         sx_triplet__1,
         sx_triplet__2,
         sx_type_name,
         sx_unary,
         sx_zeros__0,
         sx_zeros__1,
         sx_zeros__2,
         sx_zeros__3,
         sx_zeros__4,
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
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__0" c_casadi__SX__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__0
  :: DM -> IO SX
casadi__SX__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__0 :: DM -> IO SX
sx__0 = casadi__SX__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__1" c_casadi__SX__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec CDouble) -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__1
  :: Vector Double -> IO SX
casadi__SX__CONSTRUCTOR__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__1 :: Vector Double -> IO SX
sx__1 = casadi__SX__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__2" c_casadi__SX__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__2
  :: IM -> IO SX
casadi__SX__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__2 :: IM -> IO SX
sx__2 = casadi__SX__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__3" c_casadi__SX__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__3
  :: Vector Int -> IO SX
casadi__SX__CONSTRUCTOR__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__3 :: Vector Int -> IO SX
sx__3 = casadi__SX__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__4" c_casadi__SX__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec CDouble))) -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__4
  :: Vector (Vector Double) -> IO SX
casadi__SX__CONSTRUCTOR__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__4 :: Vector (Vector Double) -> IO SX
sx__4 = casadi__SX__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__5" c_casadi__SX__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> CDouble -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__5
  :: Double -> IO SX
casadi__SX__CONSTRUCTOR__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__5 :: Double -> IO SX
sx__5 = casadi__SX__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__6" c_casadi__SX__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr SX' -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__6
  :: Sparsity -> SX -> IO SX
casadi__SX__CONSTRUCTOR__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx__6 :: Sparsity -> SX -> IO SX
sx__6 = casadi__SX__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__7" c_casadi__SX__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__7
  :: Sparsity -> IO SX
casadi__SX__CONSTRUCTOR__7 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__7 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__7 :: Sparsity -> IO SX
sx__7 = casadi__SX__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__8" c_casadi__SX__CONSTRUCTOR__8
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__8
  :: Int -> Int -> IO SX
casadi__SX__CONSTRUCTOR__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx__8 :: Int -> Int -> IO SX
sx__8 = casadi__SX__CONSTRUCTOR__8


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__9" c_casadi__SX__CONSTRUCTOR__9
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__9
  :: SX -> IO SX
casadi__SX__CONSTRUCTOR__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx__9 :: SX -> IO SX
sx__9 = casadi__SX__CONSTRUCTOR__9


-- direct wrapper
foreign import ccall unsafe "casadi__SX__CONSTRUCTOR__10" c_casadi__SX__CONSTRUCTOR__10
  :: Ptr (Ptr StdString) -> IO (Ptr SX')

casadi__SX__CONSTRUCTOR__10
  :: IO SX
casadi__SX__CONSTRUCTOR__10  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__CONSTRUCTOR__10 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx__10 :: IO SX
sx__10 = casadi__SX__CONSTRUCTOR__10


-- direct wrapper
foreign import ccall unsafe "casadi__SX__T" c_casadi__SX__T
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi__SX__T
  :: SX -> IO SX
casadi__SX__T x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__T errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_T :: SXClass a => a -> IO SX
sx_T x = casadi__SX__T (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX____nonzero__" c_casadi__SX____nonzero__
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX____nonzero__
  :: SX -> IO Bool
casadi__SX____nonzero__ x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX____nonzero__ errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx___nonzero__ :: SXClass a => a -> IO Bool
sx___nonzero__ x = casadi__SX____nonzero__ (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__binary" c_casadi__SX__binary
  :: Ptr (Ptr StdString) -> CLLong -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi__SX__binary
  :: Int -> SX -> SX -> IO SX
casadi__SX__binary x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__binary errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_binary :: Int -> SX -> SX -> IO SX
sx_binary = casadi__SX__binary


-- direct wrapper
foreign import ccall unsafe "casadi__SX__clear" c_casadi__SX__clear
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO ()

casadi__SX__clear
  :: SX -> IO ()
casadi__SX__clear x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__clear errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_clear :: SXClass a => a -> IO ()
sx_clear x = casadi__SX__clear (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__colind" c_casadi__SX__colind
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> IO CLLong

casadi__SX__colind
  :: SX -> Int -> IO Int
casadi__SX__colind x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__colind errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_colind :: SXClass a => a -> Int -> IO Int
sx_colind x = casadi__SX__colind (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__columns" c_casadi__SX__columns
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__columns
  :: SX -> IO Int
casadi__SX__columns x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__columns errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_columns :: SXClass a => a -> IO Int
sx_columns x = casadi__SX__columns (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__dep__0" c_casadi__SX__dep__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi__SX__dep__0
  :: SX -> IO SX
casadi__SX__dep__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__dep__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_dep__0 :: SXClass a => a -> IO SX
sx_dep__0 x = casadi__SX__dep__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__dep__1" c_casadi__SX__dep__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> IO (Ptr SX')

casadi__SX__dep__1
  :: SX -> Int -> IO SX
casadi__SX__dep__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__dep__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_dep__1 :: SXClass a => a -> Int -> IO SX
sx_dep__1 x = casadi__SX__dep__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__dim__0" c_casadi__SX__dim__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr StdString)

casadi__SX__dim__0
  :: SX -> IO String
casadi__SX__dim__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__dim__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_dim__0 :: SXClass a => a -> IO String
sx_dim__0 x = casadi__SX__dim__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__dim__1" c_casadi__SX__dim__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr StdString)

casadi__SX__dim__1
  :: SX -> Bool -> IO String
casadi__SX__dim__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__dim__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_dim__1 :: SXClass a => a -> Bool -> IO String
sx_dim__1 x = casadi__SX__dim__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__element_hash" c_casadi__SX__element_hash
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__element_hash
  :: SX -> IO Int
casadi__SX__element_hash x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__element_hash errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_element_hash :: SXClass a => a -> IO Int
sx_element_hash x = casadi__SX__element_hash (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__enlarge__0" c_casadi__SX__enlarge__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> CLLong -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> IO ()

casadi__SX__enlarge__0
  :: SX -> Int -> Int -> Vector Int -> Vector Int -> IO ()
casadi__SX__enlarge__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__enlarge__0 errStrPtrP x0' x1' x2' x3' x4'
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
sx_enlarge__0 :: SXClass a => a -> Int -> Int -> Vector Int -> Vector Int -> IO ()
sx_enlarge__0 x = casadi__SX__enlarge__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__enlarge__1" c_casadi__SX__enlarge__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> CLLong -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> CInt -> IO ()

casadi__SX__enlarge__1
  :: SX -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__SX__enlarge__1 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__enlarge__1 errStrPtrP x0' x1' x2' x3' x4' x5'
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
sx_enlarge__1 :: SXClass a => a -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
sx_enlarge__1 x = casadi__SX__enlarge__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__erase__0" c_casadi__SX__erase__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CLLong) -> IO ()

casadi__SX__erase__0
  :: SX -> Vector Int -> IO ()
casadi__SX__erase__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__erase__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sx_erase__0 :: SXClass a => a -> Vector Int -> IO ()
sx_erase__0 x = casadi__SX__erase__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__erase__1" c_casadi__SX__erase__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CLLong) -> CInt -> IO ()

casadi__SX__erase__1
  :: SX -> Vector Int -> Bool -> IO ()
casadi__SX__erase__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__erase__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sx_erase__1 :: SXClass a => a -> Vector Int -> Bool -> IO ()
sx_erase__1 x = casadi__SX__erase__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__erase__2" c_casadi__SX__erase__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> IO ()

casadi__SX__erase__2
  :: SX -> Vector Int -> Vector Int -> IO ()
casadi__SX__erase__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__erase__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sx_erase__2 :: SXClass a => a -> Vector Int -> Vector Int -> IO ()
sx_erase__2 x = casadi__SX__erase__2 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__erase__3" c_casadi__SX__erase__3
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> CInt -> IO ()

casadi__SX__erase__3
  :: SX -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__SX__erase__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__erase__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sx_erase__3 :: SXClass a => a -> Vector Int -> Vector Int -> Bool -> IO ()
sx_erase__3 x = casadi__SX__erase__3 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__export_code" c_casadi__SX__export_code
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr StdString -> IO ()

casadi__SX__export_code
  :: SX -> String -> IO ()
casadi__SX__export_code x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__export_code errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sx_export_code :: SXClass a => a -> String -> IO ()
sx_export_code x = casadi__SX__export_code (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__eye" c_casadi__SX__eye
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr SX')

casadi__SX__eye
  :: Int -> IO SX
casadi__SX__eye x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__eye errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_eye :: Int -> IO SX
sx_eye = casadi__SX__eye


-- direct wrapper
foreign import ccall unsafe "casadi__SX__from_info" c_casadi__SX__from_info
  :: Ptr (Ptr StdString) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr SX')

casadi__SX__from_info
  :: M.Map String GenericType -> IO SX
casadi__SX__from_info x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__from_info errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_from_info :: M.Map String GenericType -> IO SX
sx_from_info = casadi__SX__from_info


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__0" c_casadi__SX__get__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__SX__get__0
  :: SX -> Bool -> IM -> IM -> IO (SX)
casadi__SX__get__0 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__0 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__0/c_casadi__SX__get__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
sx_get__0 :: SXClass a => a -> Bool -> IM -> IM -> IO (SX)
sx_get__0 x = casadi__SX__get__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__1" c_casadi__SX__get__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__SX__get__1
  :: SX -> Bool -> IM -> Slice -> IO (SX)
casadi__SX__get__1 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__1 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__1/c_casadi__SX__get__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
sx_get__1 :: SXClass a => a -> Bool -> IM -> Slice -> IO (SX)
sx_get__1 x = casadi__SX__get__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__2" c_casadi__SX__get__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__SX__get__2
  :: SX -> Bool -> Slice -> IM -> IO (SX)
casadi__SX__get__2 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__2 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__2/c_casadi__SX__get__2" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
sx_get__2 :: SXClass a => a -> Bool -> Slice -> IM -> IO (SX)
sx_get__2 x = casadi__SX__get__2 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__3" c_casadi__SX__get__3
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__SX__get__3
  :: SX -> Bool -> Slice -> Slice -> IO (SX)
casadi__SX__get__3 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__3 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__3/c_casadi__SX__get__3" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
sx_get__3 :: SXClass a => a -> Bool -> Slice -> Slice -> IO (SX)
sx_get__3 x = casadi__SX__get__3 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__4" c_casadi__SX__get__4
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr Sparsity' -> IO ()

casadi__SX__get__4
  :: SX -> Bool -> Sparsity -> IO (SX)
casadi__SX__get__4 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__4 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__4/c_casadi__SX__get__4" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
sx_get__4 :: SXClass a => a -> Bool -> Sparsity -> IO (SX)
sx_get__4 x = casadi__SX__get__4 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__5" c_casadi__SX__get__5
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr IM' -> IO ()

casadi__SX__get__5
  :: SX -> Bool -> IM -> IO (SX)
casadi__SX__get__5 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__5 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__5/c_casadi__SX__get__5" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
sx_get__5 :: SXClass a => a -> Bool -> IM -> IO (SX)
sx_get__5 x = casadi__SX__get__5 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get__6" c_casadi__SX__get__6
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr Slice' -> IO ()

casadi__SX__get__6
  :: SX -> Bool -> Slice -> IO (SX)
casadi__SX__get__6 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get__6 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get__6/c_casadi__SX__get__6" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
sx_get__6 :: SXClass a => a -> Bool -> Slice -> IO (SX)
sx_get__6 x = casadi__SX__get__6 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_colind" c_casadi__SX__get_colind
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec CLLong))

casadi__SX__get_colind
  :: SX -> IO (Vector Int)
casadi__SX__get_colind x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_colind errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_colind :: SXClass a => a -> IO (Vector Int)
sx_get_colind x = casadi__SX__get_colind (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_elements" c_casadi__SX__get_elements
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr SXElem')))

casadi__SX__get_elements
  :: SX -> IO (Vector SXElem)
casadi__SX__get_elements x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_elements errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_elements :: SXClass a => a -> IO (Vector SXElem)
sx_get_elements x = casadi__SX__get_elements (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_free" c_casadi__SX__get_free
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr SX')))

casadi__SX__get_free
  :: Function -> IO (Vector SX)
casadi__SX__get_free x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_free errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_free :: Function -> IO (Vector SX)
sx_get_free = casadi__SX__get_free


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_input" c_casadi__SX__get_input
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr SX')))

casadi__SX__get_input
  :: Function -> IO (Vector SX)
casadi__SX__get_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_input :: Function -> IO (Vector SX)
sx_get_input = casadi__SX__get_input


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_max_depth" c_casadi__SX__get_max_depth
  :: Ptr (Ptr StdString) -> IO CLLong

casadi__SX__get_max_depth
  :: IO Int
casadi__SX__get_max_depth  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_max_depth errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_get_max_depth :: IO Int
sx_get_max_depth = casadi__SX__get_max_depth


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_nonzeros" c_casadi__SX__get_nonzeros
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec (Ptr SXElem')))

casadi__SX__get_nonzeros
  :: SX -> IO (Vector SXElem)
casadi__SX__get_nonzeros x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_nonzeros errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_nonzeros :: SXClass a => a -> IO (Vector SXElem)
sx_get_nonzeros x = casadi__SX__get_nonzeros (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_nz__0" c_casadi__SX__get_nz__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr IM' -> IO ()

casadi__SX__get_nz__0
  :: SX -> Bool -> IM -> IO (SX)
casadi__SX__get_nz__0 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_nz__0 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get_nz__0/c_casadi__SX__get_nz__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
sx_get_nz__0 :: SXClass a => a -> Bool -> IM -> IO (SX)
sx_get_nz__0 x = casadi__SX__get_nz__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_nz__1" c_casadi__SX__get_nz__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr SX') -> CInt -> Ptr Slice' -> IO ()

casadi__SX__get_nz__1
  :: SX -> Bool -> Slice -> IO (SX)
casadi__SX__get_nz__1 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_nz__1 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__get_nz__1/c_casadi__SX__get_nz__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
sx_get_nz__1 :: SXClass a => a -> Bool -> Slice -> IO (SX)
sx_get_nz__1 x = casadi__SX__get_nz__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_row" c_casadi__SX__get_row
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdVec CLLong))

casadi__SX__get_row
  :: SX -> IO (Vector Int)
casadi__SX__get_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_row :: SXClass a => a -> IO (Vector Int)
sx_get_row x = casadi__SX__get_row (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_sparsity" c_casadi__SX__get_sparsity
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr Sparsity')

casadi__SX__get_sparsity
  :: SX -> IO Sparsity
casadi__SX__get_sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_sparsity :: SXClass a => a -> IO Sparsity
sx_get_sparsity x = casadi__SX__get_sparsity (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_str__0" c_casadi__SX__get_str__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr StdString)

casadi__SX__get_str__0
  :: SX -> IO String
casadi__SX__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_get_str__0 :: SXClass a => a -> IO String
sx_get_str__0 x = casadi__SX__get_str__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__get_str__1" c_casadi__SX__get_str__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO (Ptr StdString)

casadi__SX__get_str__1
  :: SX -> Bool -> IO String
casadi__SX__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_get_str__1 :: SXClass a => a -> Bool -> IO String
sx_get_str__1 x = casadi__SX__get_str__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__has_duplicates" c_casadi__SX__has_duplicates
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__has_duplicates
  :: SX -> IO Bool
casadi__SX__has_duplicates x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__has_duplicates errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_has_duplicates :: SXClass a => a -> IO Bool
sx_has_duplicates x = casadi__SX__has_duplicates (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__has_nz" c_casadi__SX__has_nz
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> CLLong -> IO CInt

casadi__SX__has_nz
  :: SX -> Int -> Int -> IO Bool
casadi__SX__has_nz x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__has_nz errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_has_nz :: SXClass a => a -> Int -> Int -> IO Bool
sx_has_nz x = casadi__SX__has_nz (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__has_zeros" c_casadi__SX__has_zeros
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__has_zeros
  :: SX -> IO Bool
casadi__SX__has_zeros x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__has_zeros errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_has_zeros :: SXClass a => a -> IO Bool
sx_has_zeros x = casadi__SX__has_zeros (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__inf__0" c_casadi__SX__inf__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__inf__0
  :: (Int, Int) -> IO SX
casadi__SX__inf__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__inf__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_inf__0 :: (Int, Int) -> IO SX
sx_inf__0 = casadi__SX__inf__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__inf__1" c_casadi__SX__inf__1
  :: Ptr (Ptr StdString) -> IO (Ptr SX')

casadi__SX__inf__1
  :: IO SX
casadi__SX__inf__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__inf__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_inf__1 :: IO SX
sx_inf__1 = casadi__SX__inf__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__inf__2" c_casadi__SX__inf__2
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr SX')

casadi__SX__inf__2
  :: Int -> IO SX
casadi__SX__inf__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__inf__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_inf__2 :: Int -> IO SX
sx_inf__2 = casadi__SX__inf__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__inf__3" c_casadi__SX__inf__3
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__inf__3
  :: Int -> Int -> IO SX
casadi__SX__inf__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__inf__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_inf__3 :: Int -> Int -> IO SX
sx_inf__3 = casadi__SX__inf__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__inf__4" c_casadi__SX__inf__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__inf__4
  :: Sparsity -> IO SX
casadi__SX__inf__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__inf__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_inf__4 :: Sparsity -> IO SX
sx_inf__4 = casadi__SX__inf__4


-- direct wrapper
foreign import ccall unsafe "casadi__SX__info" c_casadi__SX__info
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__SX__info
  :: SX -> IO (M.Map String GenericType)
casadi__SX__info x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__info errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_info :: SXClass a => a -> IO (M.Map String GenericType)
sx_info x = casadi__SX__info (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_column" c_casadi__SX__is_column
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_column
  :: SX -> IO Bool
casadi__SX__is_column x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_column errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_column :: SXClass a => a -> IO Bool
sx_is_column x = casadi__SX__is_column (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_commutative" c_casadi__SX__is_commutative
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_commutative
  :: SX -> IO Bool
casadi__SX__is_commutative x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_commutative errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_commutative :: SXClass a => a -> IO Bool
sx_is_commutative x = casadi__SX__is_commutative (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_constant" c_casadi__SX__is_constant
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_constant
  :: SX -> IO Bool
casadi__SX__is_constant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_constant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_constant :: SXClass a => a -> IO Bool
sx_is_constant x = casadi__SX__is_constant (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_dense" c_casadi__SX__is_dense
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_dense
  :: SX -> IO Bool
casadi__SX__is_dense x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_dense errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_dense :: SXClass a => a -> IO Bool
sx_is_dense x = casadi__SX__is_dense (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_empty__0" c_casadi__SX__is_empty__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_empty__0
  :: SX -> IO Bool
casadi__SX__is_empty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_empty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_empty__0 :: SXClass a => a -> IO Bool
sx_is_empty__0 x = casadi__SX__is_empty__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_empty__1" c_casadi__SX__is_empty__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO CInt

casadi__SX__is_empty__1
  :: SX -> Bool -> IO Bool
casadi__SX__is_empty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_empty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_is_empty__1 :: SXClass a => a -> Bool -> IO Bool
sx_is_empty__1 x = casadi__SX__is_empty__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_eye" c_casadi__SX__is_eye
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_eye
  :: SX -> IO Bool
casadi__SX__is_eye x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_eye errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_eye :: SXClass a => a -> IO Bool
sx_is_eye x = casadi__SX__is_eye (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_integer" c_casadi__SX__is_integer
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_integer
  :: SX -> IO Bool
casadi__SX__is_integer x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_integer errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_integer :: SXClass a => a -> IO Bool
sx_is_integer x = casadi__SX__is_integer (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_leaf" c_casadi__SX__is_leaf
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_leaf
  :: SX -> IO Bool
casadi__SX__is_leaf x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_leaf errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_leaf :: SXClass a => a -> IO Bool
sx_is_leaf x = casadi__SX__is_leaf (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_minus_one" c_casadi__SX__is_minus_one
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_minus_one
  :: SX -> IO Bool
casadi__SX__is_minus_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_minus_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_minus_one :: SXClass a => a -> IO Bool
sx_is_minus_one x = casadi__SX__is_minus_one (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_one" c_casadi__SX__is_one
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_one
  :: SX -> IO Bool
casadi__SX__is_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_one :: SXClass a => a -> IO Bool
sx_is_one x = casadi__SX__is_one (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_op" c_casadi__SX__is_op
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> IO CInt

casadi__SX__is_op
  :: SX -> Int -> IO Bool
casadi__SX__is_op x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_op errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_is_op :: SXClass a => a -> Int -> IO Bool
sx_is_op x = casadi__SX__is_op (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_regular" c_casadi__SX__is_regular
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_regular
  :: SX -> IO Bool
casadi__SX__is_regular x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_regular errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_regular :: SXClass a => a -> IO Bool
sx_is_regular x = casadi__SX__is_regular (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_row" c_casadi__SX__is_row
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_row
  :: SX -> IO Bool
casadi__SX__is_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_row :: SXClass a => a -> IO Bool
sx_is_row x = casadi__SX__is_row (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_scalar__0" c_casadi__SX__is_scalar__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_scalar__0
  :: SX -> IO Bool
casadi__SX__is_scalar__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_scalar__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_scalar__0 :: SXClass a => a -> IO Bool
sx_is_scalar__0 x = casadi__SX__is_scalar__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_scalar__1" c_casadi__SX__is_scalar__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CInt -> IO CInt

casadi__SX__is_scalar__1
  :: SX -> Bool -> IO Bool
casadi__SX__is_scalar__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_scalar__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_is_scalar__1 :: SXClass a => a -> Bool -> IO Bool
sx_is_scalar__1 x = casadi__SX__is_scalar__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_smooth" c_casadi__SX__is_smooth
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_smooth
  :: SX -> IO Bool
casadi__SX__is_smooth x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_smooth errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_smooth :: SXClass a => a -> IO Bool
sx_is_smooth x = casadi__SX__is_smooth (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_square" c_casadi__SX__is_square
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_square
  :: SX -> IO Bool
casadi__SX__is_square x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_square errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_square :: SXClass a => a -> IO Bool
sx_is_square x = casadi__SX__is_square (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_symbolic" c_casadi__SX__is_symbolic
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_symbolic
  :: SX -> IO Bool
casadi__SX__is_symbolic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_symbolic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_symbolic :: SXClass a => a -> IO Bool
sx_is_symbolic x = casadi__SX__is_symbolic (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_tril" c_casadi__SX__is_tril
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_tril
  :: SX -> IO Bool
casadi__SX__is_tril x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_tril errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_tril :: SXClass a => a -> IO Bool
sx_is_tril x = casadi__SX__is_tril (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_triu" c_casadi__SX__is_triu
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_triu
  :: SX -> IO Bool
casadi__SX__is_triu x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_triu errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_triu :: SXClass a => a -> IO Bool
sx_is_triu x = casadi__SX__is_triu (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_valid_input" c_casadi__SX__is_valid_input
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_valid_input
  :: SX -> IO Bool
casadi__SX__is_valid_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_valid_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_valid_input :: SXClass a => a -> IO Bool
sx_is_valid_input x = casadi__SX__is_valid_input (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_vector" c_casadi__SX__is_vector
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_vector
  :: SX -> IO Bool
casadi__SX__is_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_vector :: SXClass a => a -> IO Bool
sx_is_vector x = casadi__SX__is_vector (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__is_zero" c_casadi__SX__is_zero
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CInt

casadi__SX__is_zero
  :: SX -> IO Bool
casadi__SX__is_zero x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__is_zero errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_is_zero :: SXClass a => a -> IO Bool
sx_is_zero x = casadi__SX__is_zero (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__matrix_matrix" c_casadi__SX__matrix_matrix
  :: Ptr (Ptr StdString) -> CLLong -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi__SX__matrix_matrix
  :: Int -> SX -> SX -> IO SX
casadi__SX__matrix_matrix x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__matrix_matrix errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_matrix_matrix :: Int -> SX -> SX -> IO SX
sx_matrix_matrix = casadi__SX__matrix_matrix


-- direct wrapper
foreign import ccall unsafe "casadi__SX__matrix_scalar" c_casadi__SX__matrix_scalar
  :: Ptr (Ptr StdString) -> CLLong -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi__SX__matrix_scalar
  :: Int -> SX -> SX -> IO SX
casadi__SX__matrix_scalar x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__matrix_scalar errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_matrix_scalar :: Int -> SX -> SX -> IO SX
sx_matrix_scalar = casadi__SX__matrix_scalar


-- direct wrapper
foreign import ccall unsafe "casadi__SX__n_dep" c_casadi__SX__n_dep
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__n_dep
  :: SX -> IO Int
casadi__SX__n_dep x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__n_dep errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_n_dep :: SXClass a => a -> IO Int
sx_n_dep x = casadi__SX__n_dep (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__name" c_casadi__SX__name
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr StdString)

casadi__SX__name
  :: SX -> IO String
casadi__SX__name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_name :: SXClass a => a -> IO String
sx_name x = casadi__SX__name (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nan__0" c_casadi__SX__nan__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__nan__0
  :: (Int, Int) -> IO SX
casadi__SX__nan__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nan__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nan__0 :: (Int, Int) -> IO SX
sx_nan__0 = casadi__SX__nan__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nan__1" c_casadi__SX__nan__1
  :: Ptr (Ptr StdString) -> IO (Ptr SX')

casadi__SX__nan__1
  :: IO SX
casadi__SX__nan__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nan__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_nan__1 :: IO SX
sx_nan__1 = casadi__SX__nan__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nan__2" c_casadi__SX__nan__2
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr SX')

casadi__SX__nan__2
  :: Int -> IO SX
casadi__SX__nan__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nan__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nan__2 :: Int -> IO SX
sx_nan__2 = casadi__SX__nan__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nan__3" c_casadi__SX__nan__3
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__nan__3
  :: Int -> Int -> IO SX
casadi__SX__nan__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nan__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_nan__3 :: Int -> Int -> IO SX
sx_nan__3 = casadi__SX__nan__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nan__4" c_casadi__SX__nan__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__nan__4
  :: Sparsity -> IO SX
casadi__SX__nan__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nan__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nan__4 :: Sparsity -> IO SX
sx_nan__4 = casadi__SX__nan__4


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nnz" c_casadi__SX__nnz
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__nnz
  :: SX -> IO Int
casadi__SX__nnz x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nnz errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nnz :: SXClass a => a -> IO Int
sx_nnz x = casadi__SX__nnz (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nnz_diag" c_casadi__SX__nnz_diag
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__nnz_diag
  :: SX -> IO Int
casadi__SX__nnz_diag x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nnz_diag errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nnz_diag :: SXClass a => a -> IO Int
sx_nnz_diag x = casadi__SX__nnz_diag (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nnz_lower" c_casadi__SX__nnz_lower
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__nnz_lower
  :: SX -> IO Int
casadi__SX__nnz_lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nnz_lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nnz_lower :: SXClass a => a -> IO Int
sx_nnz_lower x = casadi__SX__nnz_lower (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__nnz_upper" c_casadi__SX__nnz_upper
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__nnz_upper
  :: SX -> IO Int
casadi__SX__nnz_upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__nnz_upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_nnz_upper :: SXClass a => a -> IO Int
sx_nnz_upper x = casadi__SX__nnz_upper (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__numel" c_casadi__SX__numel
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__numel
  :: SX -> IO Int
casadi__SX__numel x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__numel errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_numel :: SXClass a => a -> IO Int
sx_numel x = casadi__SX__numel (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__ones__0" c_casadi__SX__ones__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__ones__0
  :: (Int, Int) -> IO SX
casadi__SX__ones__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__ones__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_ones__0 :: (Int, Int) -> IO SX
sx_ones__0 = casadi__SX__ones__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__ones__1" c_casadi__SX__ones__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__ones__1
  :: Sparsity -> IO SX
casadi__SX__ones__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__ones__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_ones__1 :: Sparsity -> IO SX
sx_ones__1 = casadi__SX__ones__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__ones__2" c_casadi__SX__ones__2
  :: Ptr (Ptr StdString) -> IO (Ptr SX')

casadi__SX__ones__2
  :: IO SX
casadi__SX__ones__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__ones__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_ones__2 :: IO SX
sx_ones__2 = casadi__SX__ones__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__ones__3" c_casadi__SX__ones__3
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr SX')

casadi__SX__ones__3
  :: Int -> IO SX
casadi__SX__ones__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__ones__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_ones__3 :: Int -> IO SX
sx_ones__3 = casadi__SX__ones__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__ones__4" c_casadi__SX__ones__4
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__ones__4
  :: Int -> Int -> IO SX
casadi__SX__ones__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__ones__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_ones__4 :: Int -> Int -> IO SX
sx_ones__4 = casadi__SX__ones__4


-- direct wrapper
foreign import ccall unsafe "casadi__SX__op" c_casadi__SX__op
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__op
  :: SX -> IO Int
casadi__SX__op x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__op errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_op :: SXClass a => a -> IO Int
sx_op x = casadi__SX__op (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__operator__plus" c_casadi__SX__operator__plus
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi__SX__operator__plus
  :: SX -> IO SX
casadi__SX__operator__plus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__operator__plus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_operator__plus :: SXClass a => a -> IO SX
sx_operator__plus x = casadi__SX__operator__plus (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__operator__minus" c_casadi__SX__operator__minus
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr SX')

casadi__SX__operator__minus
  :: SX -> IO SX
casadi__SX__operator__minus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__operator__minus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_operator__minus :: SXClass a => a -> IO SX
sx_operator__minus x = casadi__SX__operator__minus (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__operator_casadi_int" c_casadi__SX__operator_casadi_int
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__operator_casadi_int
  :: SX -> IO Int
casadi__SX__operator_casadi_int x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__operator_casadi_int errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_operator_casadi_int :: SXClass a => a -> IO Int
sx_operator_casadi_int x = casadi__SX__operator_casadi_int (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__operator_double" c_casadi__SX__operator_double
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CDouble

casadi__SX__operator_double
  :: SX -> IO Double
casadi__SX__operator_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__operator_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_operator_double :: SXClass a => a -> IO Double
sx_operator_double x = casadi__SX__operator_double (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__print_split" c_casadi__SX__print_split
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (Ptr (StdVec (Ptr StdString))) -> Ptr (Ptr (StdVec (Ptr StdString))) -> IO ()

casadi__SX__print_split
  :: SX -> IO (Vector String, Vector String)
casadi__SX__print_split x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__print_split errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__SX__print_split/c_casadi__SX__print_split" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__SX__print_split/c_casadi__SX__print_split" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
sx_print_split :: SXClass a => a -> IO (Vector String, Vector String)
sx_print_split x = casadi__SX__print_split (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__printme" c_casadi__SX__printme
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi__SX__printme
  :: SX -> SX -> IO SX
casadi__SX__printme x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__printme errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_printme :: SXClass a => a -> SX -> IO SX
sx_printme x = casadi__SX__printme (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rand__0" c_casadi__SX__rand__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__rand__0
  :: (Int, Int) -> IO SX
casadi__SX__rand__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rand__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_rand__0 :: (Int, Int) -> IO SX
sx_rand__0 = casadi__SX__rand__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rand__1" c_casadi__SX__rand__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__rand__1
  :: Sparsity -> IO SX
casadi__SX__rand__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rand__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_rand__1 :: Sparsity -> IO SX
sx_rand__1 = casadi__SX__rand__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rand__2" c_casadi__SX__rand__2
  :: Ptr (Ptr StdString) -> IO (Ptr SX')

casadi__SX__rand__2
  :: IO SX
casadi__SX__rand__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rand__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_rand__2 :: IO SX
sx_rand__2 = casadi__SX__rand__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rand__3" c_casadi__SX__rand__3
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr SX')

casadi__SX__rand__3
  :: Int -> IO SX
casadi__SX__rand__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rand__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_rand__3 :: Int -> IO SX
sx_rand__3 = casadi__SX__rand__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rand__4" c_casadi__SX__rand__4
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__rand__4
  :: Int -> Int -> IO SX
casadi__SX__rand__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rand__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_rand__4 :: Int -> Int -> IO SX
sx_rand__4 = casadi__SX__rand__4


-- direct wrapper
foreign import ccall unsafe "casadi__SX__remove" c_casadi__SX__remove
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> IO ()

casadi__SX__remove
  :: SX -> Vector Int -> Vector Int -> IO ()
casadi__SX__remove x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__remove errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sx_remove :: SXClass a => a -> Vector Int -> Vector Int -> IO ()
sx_remove x = casadi__SX__remove (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__reserve__0" c_casadi__SX__reserve__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> CLLong -> IO ()

casadi__SX__reserve__0
  :: SX -> Int -> Int -> IO ()
casadi__SX__reserve__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__reserve__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sx_reserve__0 :: SXClass a => a -> Int -> Int -> IO ()
sx_reserve__0 x = casadi__SX__reserve__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__reserve__1" c_casadi__SX__reserve__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> IO ()

casadi__SX__reserve__1
  :: SX -> Int -> IO ()
casadi__SX__reserve__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__reserve__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sx_reserve__1 :: SXClass a => a -> Int -> IO ()
sx_reserve__1 x = casadi__SX__reserve__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__reset_input" c_casadi__SX__reset_input
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO ()

casadi__SX__reset_input
  :: SX -> IO ()
casadi__SX__reset_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__reset_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_reset_input :: SXClass a => a -> IO ()
sx_reset_input x = casadi__SX__reset_input (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__resize" c_casadi__SX__resize
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> CLLong -> IO ()

casadi__SX__resize
  :: SX -> Int -> Int -> IO ()
casadi__SX__resize x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__resize errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sx_resize :: SXClass a => a -> Int -> Int -> IO ()
sx_resize x = casadi__SX__resize (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rng" c_casadi__SX__rng
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__SX__rng
  :: Int -> IO ()
casadi__SX__rng x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rng errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_rng :: Int -> IO ()
sx_rng = casadi__SX__rng


-- direct wrapper
foreign import ccall unsafe "casadi__SX__row" c_casadi__SX__row
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> IO CLLong

casadi__SX__row
  :: SX -> Int -> IO Int
casadi__SX__row x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__row errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_row :: SXClass a => a -> Int -> IO Int
sx_row x = casadi__SX__row (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__rows" c_casadi__SX__rows
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__rows
  :: SX -> IO Int
casadi__SX__rows x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__rows errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_rows :: SXClass a => a -> IO Int
sx_rows x = casadi__SX__rows (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__scalar_matrix" c_casadi__SX__scalar_matrix
  :: Ptr (Ptr StdString) -> CLLong -> Ptr SX' -> Ptr SX' -> IO (Ptr SX')

casadi__SX__scalar_matrix
  :: Int -> SX -> SX -> IO SX
casadi__SX__scalar_matrix x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__scalar_matrix errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_scalar_matrix :: Int -> SX -> SX -> IO SX
sx_scalar_matrix = casadi__SX__scalar_matrix


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__0" c_casadi__SX__set__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__SX__set__0
  :: SX -> SX -> Bool -> IM -> IM -> IO ()
casadi__SX__set__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__0 errStrPtrP x0' x1' x2' x3' x4'
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
sx_set__0 :: SXClass a => a -> SX -> Bool -> IM -> IM -> IO ()
sx_set__0 x = casadi__SX__set__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__1" c_casadi__SX__set__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__SX__set__1
  :: SX -> SX -> Bool -> IM -> Slice -> IO ()
casadi__SX__set__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__1 errStrPtrP x0' x1' x2' x3' x4'
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
sx_set__1 :: SXClass a => a -> SX -> Bool -> IM -> Slice -> IO ()
sx_set__1 x = casadi__SX__set__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__2" c_casadi__SX__set__2
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__SX__set__2
  :: SX -> SX -> Bool -> Slice -> IM -> IO ()
casadi__SX__set__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__2 errStrPtrP x0' x1' x2' x3' x4'
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
sx_set__2 :: SXClass a => a -> SX -> Bool -> Slice -> IM -> IO ()
sx_set__2 x = casadi__SX__set__2 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__3" c_casadi__SX__set__3
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__SX__set__3
  :: SX -> SX -> Bool -> Slice -> Slice -> IO ()
casadi__SX__set__3 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__3 errStrPtrP x0' x1' x2' x3' x4'
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
sx_set__3 :: SXClass a => a -> SX -> Bool -> Slice -> Slice -> IO ()
sx_set__3 x = casadi__SX__set__3 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__4" c_casadi__SX__set__4
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr Sparsity' -> IO ()

casadi__SX__set__4
  :: SX -> SX -> Bool -> Sparsity -> IO ()
casadi__SX__set__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sx_set__4 :: SXClass a => a -> SX -> Bool -> Sparsity -> IO ()
sx_set__4 x = casadi__SX__set__4 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__5" c_casadi__SX__set__5
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr IM' -> IO ()

casadi__SX__set__5
  :: SX -> SX -> Bool -> IM -> IO ()
casadi__SX__set__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sx_set__5 :: SXClass a => a -> SX -> Bool -> IM -> IO ()
sx_set__5 x = casadi__SX__set__5 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set__6" c_casadi__SX__set__6
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr Slice' -> IO ()

casadi__SX__set__6
  :: SX -> SX -> Bool -> Slice -> IO ()
casadi__SX__set__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sx_set__6 :: SXClass a => a -> SX -> Bool -> Slice -> IO ()
sx_set__6 x = casadi__SX__set__6 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_max_depth__0" c_casadi__SX__set_max_depth__0
  :: Ptr (Ptr StdString) -> IO ()

casadi__SX__set_max_depth__0
  :: IO ()
casadi__SX__set_max_depth__0  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_max_depth__0 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ()



-- classy wrapper
sx_set_max_depth__0 :: IO ()
sx_set_max_depth__0 = casadi__SX__set_max_depth__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_max_depth__1" c_casadi__SX__set_max_depth__1
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__SX__set_max_depth__1
  :: Int -> IO ()
casadi__SX__set_max_depth__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_max_depth__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_set_max_depth__1 :: Int -> IO ()
sx_set_max_depth__1 = casadi__SX__set_max_depth__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_nz__0" c_casadi__SX__set_nz__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr IM' -> IO ()

casadi__SX__set_nz__0
  :: SX -> SX -> Bool -> IM -> IO ()
casadi__SX__set_nz__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_nz__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sx_set_nz__0 :: SXClass a => a -> SX -> Bool -> IM -> IO ()
sx_set_nz__0 x = casadi__SX__set_nz__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_nz__1" c_casadi__SX__set_nz__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr SX' -> CInt -> Ptr Slice' -> IO ()

casadi__SX__set_nz__1
  :: SX -> SX -> Bool -> Slice -> IO ()
casadi__SX__set_nz__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_nz__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
sx_set_nz__1 :: SXClass a => a -> SX -> Bool -> Slice -> IO ()
sx_set_nz__1 x = casadi__SX__set_nz__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_precision" c_casadi__SX__set_precision
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__SX__set_precision
  :: Int -> IO ()
casadi__SX__set_precision x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_precision errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_set_precision :: Int -> IO ()
sx_set_precision = casadi__SX__set_precision


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_scientific" c_casadi__SX__set_scientific
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__SX__set_scientific
  :: Bool -> IO ()
casadi__SX__set_scientific x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_scientific errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_set_scientific :: Bool -> IO ()
sx_set_scientific = casadi__SX__set_scientific


-- direct wrapper
foreign import ccall unsafe "casadi__SX__set_width" c_casadi__SX__set_width
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__SX__set_width
  :: Int -> IO ()
casadi__SX__set_width x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__set_width errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
sx_set_width :: Int -> IO ()
sx_set_width = casadi__SX__set_width


-- direct wrapper
foreign import ccall unsafe "casadi__SX__size__0" c_casadi__SX__size__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> CLLong -> IO CLLong

casadi__SX__size__0
  :: SX -> Int -> IO Int
casadi__SX__size__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__size__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_size__0 :: SXClass a => a -> Int -> IO Int
sx_size__0 x = casadi__SX__size__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__size__1" c_casadi__SX__size__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr (StdPair CLLong CLLong))

casadi__SX__size__1
  :: SX -> IO (Int, Int)
casadi__SX__size__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__size__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_size__1 :: SXClass a => a -> IO (Int, Int)
sx_size__1 x = casadi__SX__size__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__size1" c_casadi__SX__size1
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__size1
  :: SX -> IO Int
casadi__SX__size1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__size1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_size1 :: SXClass a => a -> IO Int
sx_size1 x = casadi__SX__size1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__size2" c_casadi__SX__size2
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO CLLong

casadi__SX__size2
  :: SX -> IO Int
casadi__SX__size2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__size2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_size2 :: SXClass a => a -> IO Int
sx_size2 x = casadi__SX__size2 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sparsity" c_casadi__SX__sparsity
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr Sparsity')

casadi__SX__sparsity
  :: SX -> IO Sparsity
casadi__SX__sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_sparsity :: SXClass a => a -> IO Sparsity
sx_sparsity x = casadi__SX__sparsity (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__0" c_casadi__SX__sym__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> CLLong -> CLLong -> CLLong -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi__SX__sym__0
  :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector SX))
casadi__SX__sym__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__0 errStrPtrP x0' x1' x2' x3' x4'
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
sx_sym__0 :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector SX))
sx_sym__0 = casadi__SX__sym__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__1" c_casadi__SX__sym__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CLLong -> CLLong -> IO (Ptr (StdVec (Ptr (StdVec (Ptr SX')))))

casadi__SX__sym__1
  :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector SX))
casadi__SX__sym__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sx_sym__1 :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector SX))
sx_sym__1 = casadi__SX__sym__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__2" c_casadi__SX__sym__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> CLLong -> CLLong -> IO (Ptr (StdVec (Ptr SX')))

casadi__SX__sym__2
  :: String -> Int -> Int -> Int -> IO (Vector SX)
casadi__SX__sym__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sx_sym__2 :: String -> Int -> Int -> Int -> IO (Vector SX)
sx_sym__2 = casadi__SX__sym__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__3" c_casadi__SX__sym__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CLLong -> IO (Ptr (StdVec (Ptr SX')))

casadi__SX__sym__3
  :: String -> Sparsity -> Int -> IO (Vector SX)
casadi__SX__sym__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_sym__3 :: String -> Sparsity -> Int -> IO (Vector SX)
sx_sym__3 = casadi__SX__sym__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__4" c_casadi__SX__sym__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__sym__4
  :: String -> Sparsity -> IO SX
casadi__SX__sym__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_sym__4 :: String -> Sparsity -> IO SX
sx_sym__4 = casadi__SX__sym__4


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__5" c_casadi__SX__sym__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__sym__5
  :: String -> (Int, Int) -> IO SX
casadi__SX__sym__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_sym__5 :: String -> (Int, Int) -> IO SX
sx_sym__5 = casadi__SX__sym__5


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__6" c_casadi__SX__sym__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr SX')

casadi__SX__sym__6
  :: String -> IO SX
casadi__SX__sym__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_sym__6 :: String -> IO SX
sx_sym__6 = casadi__SX__sym__6


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__7" c_casadi__SX__sym__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> IO (Ptr SX')

casadi__SX__sym__7
  :: String -> Int -> IO SX
casadi__SX__sym__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_sym__7 :: String -> Int -> IO SX
sx_sym__7 = casadi__SX__sym__7


-- direct wrapper
foreign import ccall unsafe "casadi__SX__sym__8" c_casadi__SX__sym__8
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__sym__8
  :: String -> Int -> Int -> IO SX
casadi__SX__sym__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__sym__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_sym__8 :: String -> Int -> Int -> IO SX
sx_sym__8 = casadi__SX__sym__8


-- direct wrapper
foreign import ccall unsafe "casadi__SX__to_file__0" c_casadi__SX__to_file__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr StdString -> IO ()

casadi__SX__to_file__0
  :: SX -> String -> IO ()
casadi__SX__to_file__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__to_file__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
sx_to_file__0 :: SXClass a => a -> String -> IO ()
sx_to_file__0 x = casadi__SX__to_file__0 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__to_file__1" c_casadi__SX__to_file__1
  :: Ptr (Ptr StdString) -> Ptr SX' -> Ptr StdString -> Ptr StdString -> IO ()

casadi__SX__to_file__1
  :: SX -> String -> String -> IO ()
casadi__SX__to_file__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__to_file__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
sx_to_file__1 :: SXClass a => a -> String -> String -> IO ()
sx_to_file__1 x = casadi__SX__to_file__1 (castSX x)


-- direct wrapper
foreign import ccall unsafe "casadi__SX__triplet__0" c_casadi__SX__triplet__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> Ptr SX' -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__triplet__0
  :: Vector Int -> Vector Int -> SX -> (Int, Int) -> IO SX
casadi__SX__triplet__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__triplet__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
sx_triplet__0 :: Vector Int -> Vector Int -> SX -> (Int, Int) -> IO SX
sx_triplet__0 = casadi__SX__triplet__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__triplet__1" c_casadi__SX__triplet__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> Ptr SX' -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__triplet__1
  :: Vector Int -> Vector Int -> SX -> Int -> Int -> IO SX
casadi__SX__triplet__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__triplet__1 errStrPtrP x0' x1' x2' x3' x4'
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
sx_triplet__1 :: Vector Int -> Vector Int -> SX -> Int -> Int -> IO SX
sx_triplet__1 = casadi__SX__triplet__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__triplet__2" c_casadi__SX__triplet__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> Ptr SX' -> IO (Ptr SX')

casadi__SX__triplet__2
  :: Vector Int -> Vector Int -> SX -> IO SX
casadi__SX__triplet__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__triplet__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
sx_triplet__2 :: Vector Int -> Vector Int -> SX -> IO SX
sx_triplet__2 = casadi__SX__triplet__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__type_name" c_casadi__SX__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__SX__type_name
  :: IO String
casadi__SX__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_type_name :: IO String
sx_type_name = casadi__SX__type_name


-- direct wrapper
foreign import ccall unsafe "casadi__SX__unary" c_casadi__SX__unary
  :: Ptr (Ptr StdString) -> CLLong -> Ptr SX' -> IO (Ptr SX')

casadi__SX__unary
  :: Int -> SX -> IO SX
casadi__SX__unary x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__unary errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_unary :: Int -> SX -> IO SX
sx_unary = casadi__SX__unary


-- direct wrapper
foreign import ccall unsafe "casadi__SX__zeros__0" c_casadi__SX__zeros__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr SX')

casadi__SX__zeros__0
  :: (Int, Int) -> IO SX
casadi__SX__zeros__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__zeros__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_zeros__0 :: (Int, Int) -> IO SX
sx_zeros__0 = casadi__SX__zeros__0


-- direct wrapper
foreign import ccall unsafe "casadi__SX__zeros__1" c_casadi__SX__zeros__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr SX')

casadi__SX__zeros__1
  :: Sparsity -> IO SX
casadi__SX__zeros__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__zeros__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_zeros__1 :: Sparsity -> IO SX
sx_zeros__1 = casadi__SX__zeros__1


-- direct wrapper
foreign import ccall unsafe "casadi__SX__zeros__2" c_casadi__SX__zeros__2
  :: Ptr (Ptr StdString) -> IO (Ptr SX')

casadi__SX__zeros__2
  :: IO SX
casadi__SX__zeros__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__zeros__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
sx_zeros__2 :: IO SX
sx_zeros__2 = casadi__SX__zeros__2


-- direct wrapper
foreign import ccall unsafe "casadi__SX__zeros__3" c_casadi__SX__zeros__3
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr SX')

casadi__SX__zeros__3
  :: Int -> IO SX
casadi__SX__zeros__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__zeros__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
sx_zeros__3 :: Int -> IO SX
sx_zeros__3 = casadi__SX__zeros__3


-- direct wrapper
foreign import ccall unsafe "casadi__SX__zeros__4" c_casadi__SX__zeros__4
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr SX')

casadi__SX__zeros__4
  :: Int -> Int -> IO SX
casadi__SX__zeros__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__SX__zeros__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
sx_zeros__4 :: Int -> Int -> IO SX
sx_zeros__4 = casadi__SX__zeros__4

