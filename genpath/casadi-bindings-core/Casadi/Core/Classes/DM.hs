{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.DM
       (
         DM,
         DMClass(..),
         dm_T,
         dm__0,
         dm__1,
         dm__10,
         dm__2,
         dm__3,
         dm__4,
         dm__5,
         dm__6,
         dm__7,
         dm__8,
         dm__9,
         dm___nonzero__,
         dm_binary,
         dm_clear,
         dm_colind,
         dm_columns,
         dm_dep__0,
         dm_dep__1,
         dm_dim__0,
         dm_dim__1,
         dm_element_hash,
         dm_enlarge__0,
         dm_enlarge__1,
         dm_erase__0,
         dm_erase__1,
         dm_erase__2,
         dm_erase__3,
         dm_export_code,
         dm_eye,
         dm_from_info,
         dm_get__0,
         dm_get__1,
         dm_get__2,
         dm_get__3,
         dm_get__4,
         dm_get__5,
         dm_get__6,
         dm_get_colind,
         dm_get_elements,
         dm_get_free,
         dm_get_input,
         dm_get_max_depth,
         dm_get_nonzeros,
         dm_get_nz__0,
         dm_get_nz__1,
         dm_get_row,
         dm_get_sparsity,
         dm_get_str__0,
         dm_get_str__1,
         dm_has_duplicates,
         dm_has_nz,
         dm_has_zeros,
         dm_inf__0,
         dm_inf__1,
         dm_inf__2,
         dm_inf__3,
         dm_inf__4,
         dm_info,
         dm_is_column,
         dm_is_commutative,
         dm_is_constant,
         dm_is_dense,
         dm_is_empty__0,
         dm_is_empty__1,
         dm_is_eye,
         dm_is_integer,
         dm_is_leaf,
         dm_is_minus_one,
         dm_is_one,
         dm_is_op,
         dm_is_regular,
         dm_is_row,
         dm_is_scalar__0,
         dm_is_scalar__1,
         dm_is_smooth,
         dm_is_square,
         dm_is_symbolic,
         dm_is_tril,
         dm_is_triu,
         dm_is_valid_input,
         dm_is_vector,
         dm_is_zero,
         dm_matrix_matrix,
         dm_matrix_scalar,
         dm_n_dep,
         dm_name,
         dm_nan__0,
         dm_nan__1,
         dm_nan__2,
         dm_nan__3,
         dm_nan__4,
         dm_nnz,
         dm_nnz_diag,
         dm_nnz_lower,
         dm_nnz_upper,
         dm_numel,
         dm_ones__0,
         dm_ones__1,
         dm_ones__2,
         dm_ones__3,
         dm_ones__4,
         dm_op,
         dm_operator__minus,
         dm_operator__plus,
         dm_operator_casadi_int,
         dm_operator_double,
         dm_print_split,
         dm_printme,
         dm_rand__0,
         dm_rand__1,
         dm_rand__2,
         dm_rand__3,
         dm_rand__4,
         dm_remove,
         dm_reserve__0,
         dm_reserve__1,
         dm_reset_input,
         dm_resize,
         dm_rng,
         dm_row,
         dm_rows,
         dm_scalar_matrix,
         dm_set__0,
         dm_set__1,
         dm_set__2,
         dm_set__3,
         dm_set__4,
         dm_set__5,
         dm_set__6,
         dm_set_max_depth__0,
         dm_set_max_depth__1,
         dm_set_nz__0,
         dm_set_nz__1,
         dm_set_precision,
         dm_set_scientific,
         dm_set_width,
         dm_size1,
         dm_size2,
         dm_size__0,
         dm_size__1,
         dm_sparsity,
         dm_sym__0,
         dm_sym__1,
         dm_sym__2,
         dm_sym__3,
         dm_sym__4,
         dm_sym__5,
         dm_sym__6,
         dm_sym__7,
         dm_sym__8,
         dm_to_file__0,
         dm_to_file__1,
         dm_triplet__0,
         dm_triplet__1,
         dm_triplet__2,
         dm_type_name,
         dm_unary,
         dm_zeros__0,
         dm_zeros__1,
         dm_zeros__2,
         dm_zeros__3,
         dm_zeros__4,
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
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__0" c_casadi__DM__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__0
  :: SX -> IO DM
casadi__DM__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__0 :: SX -> IO DM
dm__0 = casadi__DM__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__1" c_casadi__DM__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SXElem')) -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__1
  :: Vector SXElem -> IO DM
casadi__DM__CONSTRUCTOR__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__1 :: Vector SXElem -> IO DM
dm__1 = casadi__DM__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__2" c_casadi__DM__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__2
  :: IM -> IO DM
casadi__DM__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__2 :: IM -> IO DM
dm__2 = casadi__DM__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__3" c_casadi__DM__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__3
  :: Vector Int -> IO DM
casadi__DM__CONSTRUCTOR__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__3 :: Vector Int -> IO DM
dm__3 = casadi__DM__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__4" c_casadi__DM__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec CDouble))) -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__4
  :: Vector (Vector Double) -> IO DM
casadi__DM__CONSTRUCTOR__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__4 :: Vector (Vector Double) -> IO DM
dm__4 = casadi__DM__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__5" c_casadi__DM__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> CDouble -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__5
  :: Double -> IO DM
casadi__DM__CONSTRUCTOR__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__5 :: Double -> IO DM
dm__5 = casadi__DM__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__6" c_casadi__DM__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr DM' -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__6
  :: Sparsity -> DM -> IO DM
casadi__DM__CONSTRUCTOR__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm__6 :: Sparsity -> DM -> IO DM
dm__6 = casadi__DM__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__7" c_casadi__DM__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__7
  :: Sparsity -> IO DM
casadi__DM__CONSTRUCTOR__7 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__7 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__7 :: Sparsity -> IO DM
dm__7 = casadi__DM__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__8" c_casadi__DM__CONSTRUCTOR__8
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__8
  :: Int -> Int -> IO DM
casadi__DM__CONSTRUCTOR__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm__8 :: Int -> Int -> IO DM
dm__8 = casadi__DM__CONSTRUCTOR__8


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__9" c_casadi__DM__CONSTRUCTOR__9
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__9
  :: DM -> IO DM
casadi__DM__CONSTRUCTOR__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm__9 :: DM -> IO DM
dm__9 = casadi__DM__CONSTRUCTOR__9


-- direct wrapper
foreign import ccall unsafe "casadi__DM__CONSTRUCTOR__10" c_casadi__DM__CONSTRUCTOR__10
  :: Ptr (Ptr StdString) -> IO (Ptr DM')

casadi__DM__CONSTRUCTOR__10
  :: IO DM
casadi__DM__CONSTRUCTOR__10  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__CONSTRUCTOR__10 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm__10 :: IO DM
dm__10 = casadi__DM__CONSTRUCTOR__10


-- direct wrapper
foreign import ccall unsafe "casadi__DM__T" c_casadi__DM__T
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi__DM__T
  :: DM -> IO DM
casadi__DM__T x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__T errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_T :: DMClass a => a -> IO DM
dm_T x = casadi__DM__T (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM____nonzero__" c_casadi__DM____nonzero__
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM____nonzero__
  :: DM -> IO Bool
casadi__DM____nonzero__ x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM____nonzero__ errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm___nonzero__ :: DMClass a => a -> IO Bool
dm___nonzero__ x = casadi__DM____nonzero__ (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__binary" c_casadi__DM__binary
  :: Ptr (Ptr StdString) -> CLLong -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi__DM__binary
  :: Int -> DM -> DM -> IO DM
casadi__DM__binary x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__binary errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_binary :: Int -> DM -> DM -> IO DM
dm_binary = casadi__DM__binary


-- direct wrapper
foreign import ccall unsafe "casadi__DM__clear" c_casadi__DM__clear
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO ()

casadi__DM__clear
  :: DM -> IO ()
casadi__DM__clear x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__clear errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_clear :: DMClass a => a -> IO ()
dm_clear x = casadi__DM__clear (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__colind" c_casadi__DM__colind
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> IO CLLong

casadi__DM__colind
  :: DM -> Int -> IO Int
casadi__DM__colind x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__colind errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_colind :: DMClass a => a -> Int -> IO Int
dm_colind x = casadi__DM__colind (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__columns" c_casadi__DM__columns
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__columns
  :: DM -> IO Int
casadi__DM__columns x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__columns errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_columns :: DMClass a => a -> IO Int
dm_columns x = casadi__DM__columns (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__dep__0" c_casadi__DM__dep__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi__DM__dep__0
  :: DM -> IO DM
casadi__DM__dep__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__dep__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_dep__0 :: DMClass a => a -> IO DM
dm_dep__0 x = casadi__DM__dep__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__dep__1" c_casadi__DM__dep__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> IO (Ptr DM')

casadi__DM__dep__1
  :: DM -> Int -> IO DM
casadi__DM__dep__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__dep__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_dep__1 :: DMClass a => a -> Int -> IO DM
dm_dep__1 x = casadi__DM__dep__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__dim__0" c_casadi__DM__dim__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr StdString)

casadi__DM__dim__0
  :: DM -> IO String
casadi__DM__dim__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__dim__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_dim__0 :: DMClass a => a -> IO String
dm_dim__0 x = casadi__DM__dim__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__dim__1" c_casadi__DM__dim__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr StdString)

casadi__DM__dim__1
  :: DM -> Bool -> IO String
casadi__DM__dim__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__dim__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_dim__1 :: DMClass a => a -> Bool -> IO String
dm_dim__1 x = casadi__DM__dim__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__element_hash" c_casadi__DM__element_hash
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__element_hash
  :: DM -> IO Int
casadi__DM__element_hash x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__element_hash errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_element_hash :: DMClass a => a -> IO Int
dm_element_hash x = casadi__DM__element_hash (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__enlarge__0" c_casadi__DM__enlarge__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> CLLong -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> IO ()

casadi__DM__enlarge__0
  :: DM -> Int -> Int -> Vector Int -> Vector Int -> IO ()
casadi__DM__enlarge__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__enlarge__0 errStrPtrP x0' x1' x2' x3' x4'
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
dm_enlarge__0 :: DMClass a => a -> Int -> Int -> Vector Int -> Vector Int -> IO ()
dm_enlarge__0 x = casadi__DM__enlarge__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__enlarge__1" c_casadi__DM__enlarge__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> CLLong -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> CInt -> IO ()

casadi__DM__enlarge__1
  :: DM -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__DM__enlarge__1 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__enlarge__1 errStrPtrP x0' x1' x2' x3' x4' x5'
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
dm_enlarge__1 :: DMClass a => a -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
dm_enlarge__1 x = casadi__DM__enlarge__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__erase__0" c_casadi__DM__erase__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CLLong) -> IO ()

casadi__DM__erase__0
  :: DM -> Vector Int -> IO ()
casadi__DM__erase__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__erase__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
dm_erase__0 :: DMClass a => a -> Vector Int -> IO ()
dm_erase__0 x = casadi__DM__erase__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__erase__1" c_casadi__DM__erase__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CLLong) -> CInt -> IO ()

casadi__DM__erase__1
  :: DM -> Vector Int -> Bool -> IO ()
casadi__DM__erase__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__erase__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
dm_erase__1 :: DMClass a => a -> Vector Int -> Bool -> IO ()
dm_erase__1 x = casadi__DM__erase__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__erase__2" c_casadi__DM__erase__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> IO ()

casadi__DM__erase__2
  :: DM -> Vector Int -> Vector Int -> IO ()
casadi__DM__erase__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__erase__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
dm_erase__2 :: DMClass a => a -> Vector Int -> Vector Int -> IO ()
dm_erase__2 x = casadi__DM__erase__2 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__erase__3" c_casadi__DM__erase__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> CInt -> IO ()

casadi__DM__erase__3
  :: DM -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__DM__erase__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__erase__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
dm_erase__3 :: DMClass a => a -> Vector Int -> Vector Int -> Bool -> IO ()
dm_erase__3 x = casadi__DM__erase__3 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__export_code" c_casadi__DM__export_code
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr StdString -> IO ()

casadi__DM__export_code
  :: DM -> String -> IO ()
casadi__DM__export_code x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__export_code errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
dm_export_code :: DMClass a => a -> String -> IO ()
dm_export_code x = casadi__DM__export_code (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__eye" c_casadi__DM__eye
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr DM')

casadi__DM__eye
  :: Int -> IO DM
casadi__DM__eye x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__eye errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_eye :: Int -> IO DM
dm_eye = casadi__DM__eye


-- direct wrapper
foreign import ccall unsafe "casadi__DM__from_info" c_casadi__DM__from_info
  :: Ptr (Ptr StdString) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr DM')

casadi__DM__from_info
  :: M.Map String GenericType -> IO DM
casadi__DM__from_info x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__from_info errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_from_info :: M.Map String GenericType -> IO DM
dm_from_info = casadi__DM__from_info


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__0" c_casadi__DM__get__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__DM__get__0
  :: DM -> Bool -> IM -> IM -> IO (DM)
casadi__DM__get__0 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__0 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__0/c_casadi__DM__get__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
dm_get__0 :: DMClass a => a -> Bool -> IM -> IM -> IO (DM)
dm_get__0 x = casadi__DM__get__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__1" c_casadi__DM__get__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__DM__get__1
  :: DM -> Bool -> IM -> Slice -> IO (DM)
casadi__DM__get__1 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__1 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__1/c_casadi__DM__get__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
dm_get__1 :: DMClass a => a -> Bool -> IM -> Slice -> IO (DM)
dm_get__1 x = casadi__DM__get__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__2" c_casadi__DM__get__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__DM__get__2
  :: DM -> Bool -> Slice -> IM -> IO (DM)
casadi__DM__get__2 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__2 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__2/c_casadi__DM__get__2" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
dm_get__2 :: DMClass a => a -> Bool -> Slice -> IM -> IO (DM)
dm_get__2 x = casadi__DM__get__2 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__3" c_casadi__DM__get__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__DM__get__3
  :: DM -> Bool -> Slice -> Slice -> IO (DM)
casadi__DM__get__3 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__3 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__3/c_casadi__DM__get__3" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
dm_get__3 :: DMClass a => a -> Bool -> Slice -> Slice -> IO (DM)
dm_get__3 x = casadi__DM__get__3 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__4" c_casadi__DM__get__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr Sparsity' -> IO ()

casadi__DM__get__4
  :: DM -> Bool -> Sparsity -> IO (DM)
casadi__DM__get__4 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__4 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__4/c_casadi__DM__get__4" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
dm_get__4 :: DMClass a => a -> Bool -> Sparsity -> IO (DM)
dm_get__4 x = casadi__DM__get__4 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__5" c_casadi__DM__get__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr IM' -> IO ()

casadi__DM__get__5
  :: DM -> Bool -> IM -> IO (DM)
casadi__DM__get__5 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__5 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__5/c_casadi__DM__get__5" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
dm_get__5 :: DMClass a => a -> Bool -> IM -> IO (DM)
dm_get__5 x = casadi__DM__get__5 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get__6" c_casadi__DM__get__6
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr Slice' -> IO ()

casadi__DM__get__6
  :: DM -> Bool -> Slice -> IO (DM)
casadi__DM__get__6 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get__6 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get__6/c_casadi__DM__get__6" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
dm_get__6 :: DMClass a => a -> Bool -> Slice -> IO (DM)
dm_get__6 x = casadi__DM__get__6 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_colind" c_casadi__DM__get_colind
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec CLLong))

casadi__DM__get_colind
  :: DM -> IO (Vector Int)
casadi__DM__get_colind x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_colind errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_colind :: DMClass a => a -> IO (Vector Int)
dm_get_colind x = casadi__DM__get_colind (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_elements" c_casadi__DM__get_elements
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec CDouble))

casadi__DM__get_elements
  :: DM -> IO (Vector Double)
casadi__DM__get_elements x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_elements errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_elements :: DMClass a => a -> IO (Vector Double)
dm_get_elements x = casadi__DM__get_elements (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_free" c_casadi__DM__get_free
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr DM')))

casadi__DM__get_free
  :: Function -> IO (Vector DM)
casadi__DM__get_free x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_free errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_free :: Function -> IO (Vector DM)
dm_get_free = casadi__DM__get_free


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_input" c_casadi__DM__get_input
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr DM')))

casadi__DM__get_input
  :: Function -> IO (Vector DM)
casadi__DM__get_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_input :: Function -> IO (Vector DM)
dm_get_input = casadi__DM__get_input


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_max_depth" c_casadi__DM__get_max_depth
  :: Ptr (Ptr StdString) -> IO CLLong

casadi__DM__get_max_depth
  :: IO Int
casadi__DM__get_max_depth  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_max_depth errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_get_max_depth :: IO Int
dm_get_max_depth = casadi__DM__get_max_depth


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_nonzeros" c_casadi__DM__get_nonzeros
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec CDouble))

casadi__DM__get_nonzeros
  :: DM -> IO (Vector Double)
casadi__DM__get_nonzeros x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_nonzeros errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_nonzeros :: DMClass a => a -> IO (Vector Double)
dm_get_nonzeros x = casadi__DM__get_nonzeros (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_nz__0" c_casadi__DM__get_nz__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr IM' -> IO ()

casadi__DM__get_nz__0
  :: DM -> Bool -> IM -> IO (DM)
casadi__DM__get_nz__0 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_nz__0 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get_nz__0/c_casadi__DM__get_nz__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
dm_get_nz__0 :: DMClass a => a -> Bool -> IM -> IO (DM)
dm_get_nz__0 x = casadi__DM__get_nz__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_nz__1" c_casadi__DM__get_nz__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr DM') -> CInt -> Ptr Slice' -> IO ()

casadi__DM__get_nz__1
  :: DM -> Bool -> Slice -> IO (DM)
casadi__DM__get_nz__1 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_nz__1 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__get_nz__1/c_casadi__DM__get_nz__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
dm_get_nz__1 :: DMClass a => a -> Bool -> Slice -> IO (DM)
dm_get_nz__1 x = casadi__DM__get_nz__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_row" c_casadi__DM__get_row
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdVec CLLong))

casadi__DM__get_row
  :: DM -> IO (Vector Int)
casadi__DM__get_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_row :: DMClass a => a -> IO (Vector Int)
dm_get_row x = casadi__DM__get_row (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_sparsity" c_casadi__DM__get_sparsity
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr Sparsity')

casadi__DM__get_sparsity
  :: DM -> IO Sparsity
casadi__DM__get_sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_sparsity :: DMClass a => a -> IO Sparsity
dm_get_sparsity x = casadi__DM__get_sparsity (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_str__0" c_casadi__DM__get_str__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr StdString)

casadi__DM__get_str__0
  :: DM -> IO String
casadi__DM__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_get_str__0 :: DMClass a => a -> IO String
dm_get_str__0 x = casadi__DM__get_str__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__get_str__1" c_casadi__DM__get_str__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO (Ptr StdString)

casadi__DM__get_str__1
  :: DM -> Bool -> IO String
casadi__DM__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_get_str__1 :: DMClass a => a -> Bool -> IO String
dm_get_str__1 x = casadi__DM__get_str__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__has_duplicates" c_casadi__DM__has_duplicates
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__has_duplicates
  :: DM -> IO Bool
casadi__DM__has_duplicates x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__has_duplicates errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_has_duplicates :: DMClass a => a -> IO Bool
dm_has_duplicates x = casadi__DM__has_duplicates (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__has_nz" c_casadi__DM__has_nz
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> CLLong -> IO CInt

casadi__DM__has_nz
  :: DM -> Int -> Int -> IO Bool
casadi__DM__has_nz x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__has_nz errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_has_nz :: DMClass a => a -> Int -> Int -> IO Bool
dm_has_nz x = casadi__DM__has_nz (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__has_zeros" c_casadi__DM__has_zeros
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__has_zeros
  :: DM -> IO Bool
casadi__DM__has_zeros x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__has_zeros errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_has_zeros :: DMClass a => a -> IO Bool
dm_has_zeros x = casadi__DM__has_zeros (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__inf__0" c_casadi__DM__inf__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__inf__0
  :: (Int, Int) -> IO DM
casadi__DM__inf__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__inf__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_inf__0 :: (Int, Int) -> IO DM
dm_inf__0 = casadi__DM__inf__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__inf__1" c_casadi__DM__inf__1
  :: Ptr (Ptr StdString) -> IO (Ptr DM')

casadi__DM__inf__1
  :: IO DM
casadi__DM__inf__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__inf__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_inf__1 :: IO DM
dm_inf__1 = casadi__DM__inf__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__inf__2" c_casadi__DM__inf__2
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr DM')

casadi__DM__inf__2
  :: Int -> IO DM
casadi__DM__inf__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__inf__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_inf__2 :: Int -> IO DM
dm_inf__2 = casadi__DM__inf__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__inf__3" c_casadi__DM__inf__3
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__inf__3
  :: Int -> Int -> IO DM
casadi__DM__inf__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__inf__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_inf__3 :: Int -> Int -> IO DM
dm_inf__3 = casadi__DM__inf__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__inf__4" c_casadi__DM__inf__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__inf__4
  :: Sparsity -> IO DM
casadi__DM__inf__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__inf__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_inf__4 :: Sparsity -> IO DM
dm_inf__4 = casadi__DM__inf__4


-- direct wrapper
foreign import ccall unsafe "casadi__DM__info" c_casadi__DM__info
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__DM__info
  :: DM -> IO (M.Map String GenericType)
casadi__DM__info x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__info errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_info :: DMClass a => a -> IO (M.Map String GenericType)
dm_info x = casadi__DM__info (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_column" c_casadi__DM__is_column
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_column
  :: DM -> IO Bool
casadi__DM__is_column x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_column errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_column :: DMClass a => a -> IO Bool
dm_is_column x = casadi__DM__is_column (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_commutative" c_casadi__DM__is_commutative
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_commutative
  :: DM -> IO Bool
casadi__DM__is_commutative x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_commutative errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_commutative :: DMClass a => a -> IO Bool
dm_is_commutative x = casadi__DM__is_commutative (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_constant" c_casadi__DM__is_constant
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_constant
  :: DM -> IO Bool
casadi__DM__is_constant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_constant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_constant :: DMClass a => a -> IO Bool
dm_is_constant x = casadi__DM__is_constant (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_dense" c_casadi__DM__is_dense
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_dense
  :: DM -> IO Bool
casadi__DM__is_dense x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_dense errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_dense :: DMClass a => a -> IO Bool
dm_is_dense x = casadi__DM__is_dense (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_empty__0" c_casadi__DM__is_empty__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_empty__0
  :: DM -> IO Bool
casadi__DM__is_empty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_empty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_empty__0 :: DMClass a => a -> IO Bool
dm_is_empty__0 x = casadi__DM__is_empty__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_empty__1" c_casadi__DM__is_empty__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO CInt

casadi__DM__is_empty__1
  :: DM -> Bool -> IO Bool
casadi__DM__is_empty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_empty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_is_empty__1 :: DMClass a => a -> Bool -> IO Bool
dm_is_empty__1 x = casadi__DM__is_empty__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_eye" c_casadi__DM__is_eye
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_eye
  :: DM -> IO Bool
casadi__DM__is_eye x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_eye errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_eye :: DMClass a => a -> IO Bool
dm_is_eye x = casadi__DM__is_eye (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_integer" c_casadi__DM__is_integer
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_integer
  :: DM -> IO Bool
casadi__DM__is_integer x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_integer errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_integer :: DMClass a => a -> IO Bool
dm_is_integer x = casadi__DM__is_integer (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_leaf" c_casadi__DM__is_leaf
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_leaf
  :: DM -> IO Bool
casadi__DM__is_leaf x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_leaf errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_leaf :: DMClass a => a -> IO Bool
dm_is_leaf x = casadi__DM__is_leaf (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_minus_one" c_casadi__DM__is_minus_one
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_minus_one
  :: DM -> IO Bool
casadi__DM__is_minus_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_minus_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_minus_one :: DMClass a => a -> IO Bool
dm_is_minus_one x = casadi__DM__is_minus_one (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_one" c_casadi__DM__is_one
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_one
  :: DM -> IO Bool
casadi__DM__is_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_one :: DMClass a => a -> IO Bool
dm_is_one x = casadi__DM__is_one (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_op" c_casadi__DM__is_op
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> IO CInt

casadi__DM__is_op
  :: DM -> Int -> IO Bool
casadi__DM__is_op x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_op errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_is_op :: DMClass a => a -> Int -> IO Bool
dm_is_op x = casadi__DM__is_op (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_regular" c_casadi__DM__is_regular
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_regular
  :: DM -> IO Bool
casadi__DM__is_regular x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_regular errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_regular :: DMClass a => a -> IO Bool
dm_is_regular x = casadi__DM__is_regular (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_row" c_casadi__DM__is_row
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_row
  :: DM -> IO Bool
casadi__DM__is_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_row :: DMClass a => a -> IO Bool
dm_is_row x = casadi__DM__is_row (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_scalar__0" c_casadi__DM__is_scalar__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_scalar__0
  :: DM -> IO Bool
casadi__DM__is_scalar__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_scalar__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_scalar__0 :: DMClass a => a -> IO Bool
dm_is_scalar__0 x = casadi__DM__is_scalar__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_scalar__1" c_casadi__DM__is_scalar__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CInt -> IO CInt

casadi__DM__is_scalar__1
  :: DM -> Bool -> IO Bool
casadi__DM__is_scalar__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_scalar__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_is_scalar__1 :: DMClass a => a -> Bool -> IO Bool
dm_is_scalar__1 x = casadi__DM__is_scalar__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_smooth" c_casadi__DM__is_smooth
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_smooth
  :: DM -> IO Bool
casadi__DM__is_smooth x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_smooth errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_smooth :: DMClass a => a -> IO Bool
dm_is_smooth x = casadi__DM__is_smooth (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_square" c_casadi__DM__is_square
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_square
  :: DM -> IO Bool
casadi__DM__is_square x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_square errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_square :: DMClass a => a -> IO Bool
dm_is_square x = casadi__DM__is_square (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_symbolic" c_casadi__DM__is_symbolic
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_symbolic
  :: DM -> IO Bool
casadi__DM__is_symbolic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_symbolic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_symbolic :: DMClass a => a -> IO Bool
dm_is_symbolic x = casadi__DM__is_symbolic (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_tril" c_casadi__DM__is_tril
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_tril
  :: DM -> IO Bool
casadi__DM__is_tril x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_tril errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_tril :: DMClass a => a -> IO Bool
dm_is_tril x = casadi__DM__is_tril (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_triu" c_casadi__DM__is_triu
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_triu
  :: DM -> IO Bool
casadi__DM__is_triu x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_triu errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_triu :: DMClass a => a -> IO Bool
dm_is_triu x = casadi__DM__is_triu (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_valid_input" c_casadi__DM__is_valid_input
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_valid_input
  :: DM -> IO Bool
casadi__DM__is_valid_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_valid_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_valid_input :: DMClass a => a -> IO Bool
dm_is_valid_input x = casadi__DM__is_valid_input (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_vector" c_casadi__DM__is_vector
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_vector
  :: DM -> IO Bool
casadi__DM__is_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_vector :: DMClass a => a -> IO Bool
dm_is_vector x = casadi__DM__is_vector (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__is_zero" c_casadi__DM__is_zero
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CInt

casadi__DM__is_zero
  :: DM -> IO Bool
casadi__DM__is_zero x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__is_zero errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_is_zero :: DMClass a => a -> IO Bool
dm_is_zero x = casadi__DM__is_zero (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__matrix_matrix" c_casadi__DM__matrix_matrix
  :: Ptr (Ptr StdString) -> CLLong -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi__DM__matrix_matrix
  :: Int -> DM -> DM -> IO DM
casadi__DM__matrix_matrix x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__matrix_matrix errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_matrix_matrix :: Int -> DM -> DM -> IO DM
dm_matrix_matrix = casadi__DM__matrix_matrix


-- direct wrapper
foreign import ccall unsafe "casadi__DM__matrix_scalar" c_casadi__DM__matrix_scalar
  :: Ptr (Ptr StdString) -> CLLong -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi__DM__matrix_scalar
  :: Int -> DM -> DM -> IO DM
casadi__DM__matrix_scalar x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__matrix_scalar errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_matrix_scalar :: Int -> DM -> DM -> IO DM
dm_matrix_scalar = casadi__DM__matrix_scalar


-- direct wrapper
foreign import ccall unsafe "casadi__DM__n_dep" c_casadi__DM__n_dep
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__n_dep
  :: DM -> IO Int
casadi__DM__n_dep x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__n_dep errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_n_dep :: DMClass a => a -> IO Int
dm_n_dep x = casadi__DM__n_dep (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__name" c_casadi__DM__name
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr StdString)

casadi__DM__name
  :: DM -> IO String
casadi__DM__name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_name :: DMClass a => a -> IO String
dm_name x = casadi__DM__name (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nan__0" c_casadi__DM__nan__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__nan__0
  :: (Int, Int) -> IO DM
casadi__DM__nan__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nan__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nan__0 :: (Int, Int) -> IO DM
dm_nan__0 = casadi__DM__nan__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nan__1" c_casadi__DM__nan__1
  :: Ptr (Ptr StdString) -> IO (Ptr DM')

casadi__DM__nan__1
  :: IO DM
casadi__DM__nan__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nan__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_nan__1 :: IO DM
dm_nan__1 = casadi__DM__nan__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nan__2" c_casadi__DM__nan__2
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr DM')

casadi__DM__nan__2
  :: Int -> IO DM
casadi__DM__nan__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nan__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nan__2 :: Int -> IO DM
dm_nan__2 = casadi__DM__nan__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nan__3" c_casadi__DM__nan__3
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__nan__3
  :: Int -> Int -> IO DM
casadi__DM__nan__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nan__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_nan__3 :: Int -> Int -> IO DM
dm_nan__3 = casadi__DM__nan__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nan__4" c_casadi__DM__nan__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__nan__4
  :: Sparsity -> IO DM
casadi__DM__nan__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nan__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nan__4 :: Sparsity -> IO DM
dm_nan__4 = casadi__DM__nan__4


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nnz" c_casadi__DM__nnz
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__nnz
  :: DM -> IO Int
casadi__DM__nnz x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nnz errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nnz :: DMClass a => a -> IO Int
dm_nnz x = casadi__DM__nnz (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nnz_diag" c_casadi__DM__nnz_diag
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__nnz_diag
  :: DM -> IO Int
casadi__DM__nnz_diag x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nnz_diag errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nnz_diag :: DMClass a => a -> IO Int
dm_nnz_diag x = casadi__DM__nnz_diag (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nnz_lower" c_casadi__DM__nnz_lower
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__nnz_lower
  :: DM -> IO Int
casadi__DM__nnz_lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nnz_lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nnz_lower :: DMClass a => a -> IO Int
dm_nnz_lower x = casadi__DM__nnz_lower (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__nnz_upper" c_casadi__DM__nnz_upper
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__nnz_upper
  :: DM -> IO Int
casadi__DM__nnz_upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__nnz_upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_nnz_upper :: DMClass a => a -> IO Int
dm_nnz_upper x = casadi__DM__nnz_upper (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__numel" c_casadi__DM__numel
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__numel
  :: DM -> IO Int
casadi__DM__numel x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__numel errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_numel :: DMClass a => a -> IO Int
dm_numel x = casadi__DM__numel (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__ones__0" c_casadi__DM__ones__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__ones__0
  :: (Int, Int) -> IO DM
casadi__DM__ones__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__ones__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_ones__0 :: (Int, Int) -> IO DM
dm_ones__0 = casadi__DM__ones__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__ones__1" c_casadi__DM__ones__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__ones__1
  :: Sparsity -> IO DM
casadi__DM__ones__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__ones__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_ones__1 :: Sparsity -> IO DM
dm_ones__1 = casadi__DM__ones__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__ones__2" c_casadi__DM__ones__2
  :: Ptr (Ptr StdString) -> IO (Ptr DM')

casadi__DM__ones__2
  :: IO DM
casadi__DM__ones__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__ones__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_ones__2 :: IO DM
dm_ones__2 = casadi__DM__ones__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__ones__3" c_casadi__DM__ones__3
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr DM')

casadi__DM__ones__3
  :: Int -> IO DM
casadi__DM__ones__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__ones__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_ones__3 :: Int -> IO DM
dm_ones__3 = casadi__DM__ones__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__ones__4" c_casadi__DM__ones__4
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__ones__4
  :: Int -> Int -> IO DM
casadi__DM__ones__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__ones__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_ones__4 :: Int -> Int -> IO DM
dm_ones__4 = casadi__DM__ones__4


-- direct wrapper
foreign import ccall unsafe "casadi__DM__op" c_casadi__DM__op
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__op
  :: DM -> IO Int
casadi__DM__op x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__op errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_op :: DMClass a => a -> IO Int
dm_op x = casadi__DM__op (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__operator__plus" c_casadi__DM__operator__plus
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi__DM__operator__plus
  :: DM -> IO DM
casadi__DM__operator__plus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__operator__plus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_operator__plus :: DMClass a => a -> IO DM
dm_operator__plus x = casadi__DM__operator__plus (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__operator__minus" c_casadi__DM__operator__minus
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr DM')

casadi__DM__operator__minus
  :: DM -> IO DM
casadi__DM__operator__minus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__operator__minus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_operator__minus :: DMClass a => a -> IO DM
dm_operator__minus x = casadi__DM__operator__minus (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__operator_casadi_int" c_casadi__DM__operator_casadi_int
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__operator_casadi_int
  :: DM -> IO Int
casadi__DM__operator_casadi_int x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__operator_casadi_int errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_operator_casadi_int :: DMClass a => a -> IO Int
dm_operator_casadi_int x = casadi__DM__operator_casadi_int (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__operator_double" c_casadi__DM__operator_double
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CDouble

casadi__DM__operator_double
  :: DM -> IO Double
casadi__DM__operator_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__operator_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_operator_double :: DMClass a => a -> IO Double
dm_operator_double x = casadi__DM__operator_double (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__print_split" c_casadi__DM__print_split
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (Ptr (StdVec (Ptr StdString))) -> Ptr (Ptr (StdVec (Ptr StdString))) -> IO ()

casadi__DM__print_split
  :: DM -> IO (Vector String, Vector String)
casadi__DM__print_split x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__print_split errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__DM__print_split/c_casadi__DM__print_split" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__DM__print_split/c_casadi__DM__print_split" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
dm_print_split :: DMClass a => a -> IO (Vector String, Vector String)
dm_print_split x = casadi__DM__print_split (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__printme" c_casadi__DM__printme
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi__DM__printme
  :: DM -> DM -> IO DM
casadi__DM__printme x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__printme errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_printme :: DMClass a => a -> DM -> IO DM
dm_printme x = casadi__DM__printme (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rand__0" c_casadi__DM__rand__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__rand__0
  :: (Int, Int) -> IO DM
casadi__DM__rand__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rand__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_rand__0 :: (Int, Int) -> IO DM
dm_rand__0 = casadi__DM__rand__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rand__1" c_casadi__DM__rand__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__rand__1
  :: Sparsity -> IO DM
casadi__DM__rand__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rand__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_rand__1 :: Sparsity -> IO DM
dm_rand__1 = casadi__DM__rand__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rand__2" c_casadi__DM__rand__2
  :: Ptr (Ptr StdString) -> IO (Ptr DM')

casadi__DM__rand__2
  :: IO DM
casadi__DM__rand__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rand__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_rand__2 :: IO DM
dm_rand__2 = casadi__DM__rand__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rand__3" c_casadi__DM__rand__3
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr DM')

casadi__DM__rand__3
  :: Int -> IO DM
casadi__DM__rand__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rand__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_rand__3 :: Int -> IO DM
dm_rand__3 = casadi__DM__rand__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rand__4" c_casadi__DM__rand__4
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__rand__4
  :: Int -> Int -> IO DM
casadi__DM__rand__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rand__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_rand__4 :: Int -> Int -> IO DM
dm_rand__4 = casadi__DM__rand__4


-- direct wrapper
foreign import ccall unsafe "casadi__DM__remove" c_casadi__DM__remove
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> IO ()

casadi__DM__remove
  :: DM -> Vector Int -> Vector Int -> IO ()
casadi__DM__remove x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__remove errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
dm_remove :: DMClass a => a -> Vector Int -> Vector Int -> IO ()
dm_remove x = casadi__DM__remove (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__reserve__0" c_casadi__DM__reserve__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> CLLong -> IO ()

casadi__DM__reserve__0
  :: DM -> Int -> Int -> IO ()
casadi__DM__reserve__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__reserve__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
dm_reserve__0 :: DMClass a => a -> Int -> Int -> IO ()
dm_reserve__0 x = casadi__DM__reserve__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__reserve__1" c_casadi__DM__reserve__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> IO ()

casadi__DM__reserve__1
  :: DM -> Int -> IO ()
casadi__DM__reserve__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__reserve__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
dm_reserve__1 :: DMClass a => a -> Int -> IO ()
dm_reserve__1 x = casadi__DM__reserve__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__reset_input" c_casadi__DM__reset_input
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO ()

casadi__DM__reset_input
  :: DM -> IO ()
casadi__DM__reset_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__reset_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_reset_input :: DMClass a => a -> IO ()
dm_reset_input x = casadi__DM__reset_input (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__resize" c_casadi__DM__resize
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> CLLong -> IO ()

casadi__DM__resize
  :: DM -> Int -> Int -> IO ()
casadi__DM__resize x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__resize errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
dm_resize :: DMClass a => a -> Int -> Int -> IO ()
dm_resize x = casadi__DM__resize (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rng" c_casadi__DM__rng
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__DM__rng
  :: Int -> IO ()
casadi__DM__rng x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rng errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_rng :: Int -> IO ()
dm_rng = casadi__DM__rng


-- direct wrapper
foreign import ccall unsafe "casadi__DM__row" c_casadi__DM__row
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> IO CLLong

casadi__DM__row
  :: DM -> Int -> IO Int
casadi__DM__row x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__row errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_row :: DMClass a => a -> Int -> IO Int
dm_row x = casadi__DM__row (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__rows" c_casadi__DM__rows
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__rows
  :: DM -> IO Int
casadi__DM__rows x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__rows errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_rows :: DMClass a => a -> IO Int
dm_rows x = casadi__DM__rows (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__scalar_matrix" c_casadi__DM__scalar_matrix
  :: Ptr (Ptr StdString) -> CLLong -> Ptr DM' -> Ptr DM' -> IO (Ptr DM')

casadi__DM__scalar_matrix
  :: Int -> DM -> DM -> IO DM
casadi__DM__scalar_matrix x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__scalar_matrix errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_scalar_matrix :: Int -> DM -> DM -> IO DM
dm_scalar_matrix = casadi__DM__scalar_matrix


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__0" c_casadi__DM__set__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__DM__set__0
  :: DM -> DM -> Bool -> IM -> IM -> IO ()
casadi__DM__set__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__0 errStrPtrP x0' x1' x2' x3' x4'
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
dm_set__0 :: DMClass a => a -> DM -> Bool -> IM -> IM -> IO ()
dm_set__0 x = casadi__DM__set__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__1" c_casadi__DM__set__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__DM__set__1
  :: DM -> DM -> Bool -> IM -> Slice -> IO ()
casadi__DM__set__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__1 errStrPtrP x0' x1' x2' x3' x4'
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
dm_set__1 :: DMClass a => a -> DM -> Bool -> IM -> Slice -> IO ()
dm_set__1 x = casadi__DM__set__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__2" c_casadi__DM__set__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__DM__set__2
  :: DM -> DM -> Bool -> Slice -> IM -> IO ()
casadi__DM__set__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__2 errStrPtrP x0' x1' x2' x3' x4'
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
dm_set__2 :: DMClass a => a -> DM -> Bool -> Slice -> IM -> IO ()
dm_set__2 x = casadi__DM__set__2 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__3" c_casadi__DM__set__3
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__DM__set__3
  :: DM -> DM -> Bool -> Slice -> Slice -> IO ()
casadi__DM__set__3 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__3 errStrPtrP x0' x1' x2' x3' x4'
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
dm_set__3 :: DMClass a => a -> DM -> Bool -> Slice -> Slice -> IO ()
dm_set__3 x = casadi__DM__set__3 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__4" c_casadi__DM__set__4
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr Sparsity' -> IO ()

casadi__DM__set__4
  :: DM -> DM -> Bool -> Sparsity -> IO ()
casadi__DM__set__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
dm_set__4 :: DMClass a => a -> DM -> Bool -> Sparsity -> IO ()
dm_set__4 x = casadi__DM__set__4 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__5" c_casadi__DM__set__5
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr IM' -> IO ()

casadi__DM__set__5
  :: DM -> DM -> Bool -> IM -> IO ()
casadi__DM__set__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
dm_set__5 :: DMClass a => a -> DM -> Bool -> IM -> IO ()
dm_set__5 x = casadi__DM__set__5 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set__6" c_casadi__DM__set__6
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr Slice' -> IO ()

casadi__DM__set__6
  :: DM -> DM -> Bool -> Slice -> IO ()
casadi__DM__set__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
dm_set__6 :: DMClass a => a -> DM -> Bool -> Slice -> IO ()
dm_set__6 x = casadi__DM__set__6 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_max_depth__0" c_casadi__DM__set_max_depth__0
  :: Ptr (Ptr StdString) -> IO ()

casadi__DM__set_max_depth__0
  :: IO ()
casadi__DM__set_max_depth__0  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_max_depth__0 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ()



-- classy wrapper
dm_set_max_depth__0 :: IO ()
dm_set_max_depth__0 = casadi__DM__set_max_depth__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_max_depth__1" c_casadi__DM__set_max_depth__1
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__DM__set_max_depth__1
  :: Int -> IO ()
casadi__DM__set_max_depth__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_max_depth__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_set_max_depth__1 :: Int -> IO ()
dm_set_max_depth__1 = casadi__DM__set_max_depth__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_nz__0" c_casadi__DM__set_nz__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr IM' -> IO ()

casadi__DM__set_nz__0
  :: DM -> DM -> Bool -> IM -> IO ()
casadi__DM__set_nz__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_nz__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
dm_set_nz__0 :: DMClass a => a -> DM -> Bool -> IM -> IO ()
dm_set_nz__0 x = casadi__DM__set_nz__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_nz__1" c_casadi__DM__set_nz__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr DM' -> CInt -> Ptr Slice' -> IO ()

casadi__DM__set_nz__1
  :: DM -> DM -> Bool -> Slice -> IO ()
casadi__DM__set_nz__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_nz__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
dm_set_nz__1 :: DMClass a => a -> DM -> Bool -> Slice -> IO ()
dm_set_nz__1 x = casadi__DM__set_nz__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_precision" c_casadi__DM__set_precision
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__DM__set_precision
  :: Int -> IO ()
casadi__DM__set_precision x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_precision errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_set_precision :: Int -> IO ()
dm_set_precision = casadi__DM__set_precision


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_scientific" c_casadi__DM__set_scientific
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__DM__set_scientific
  :: Bool -> IO ()
casadi__DM__set_scientific x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_scientific errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_set_scientific :: Bool -> IO ()
dm_set_scientific = casadi__DM__set_scientific


-- direct wrapper
foreign import ccall unsafe "casadi__DM__set_width" c_casadi__DM__set_width
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__DM__set_width
  :: Int -> IO ()
casadi__DM__set_width x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__set_width errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
dm_set_width :: Int -> IO ()
dm_set_width = casadi__DM__set_width


-- direct wrapper
foreign import ccall unsafe "casadi__DM__size__0" c_casadi__DM__size__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> CLLong -> IO CLLong

casadi__DM__size__0
  :: DM -> Int -> IO Int
casadi__DM__size__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__size__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_size__0 :: DMClass a => a -> Int -> IO Int
dm_size__0 x = casadi__DM__size__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__size__1" c_casadi__DM__size__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr (StdPair CLLong CLLong))

casadi__DM__size__1
  :: DM -> IO (Int, Int)
casadi__DM__size__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__size__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_size__1 :: DMClass a => a -> IO (Int, Int)
dm_size__1 x = casadi__DM__size__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__size1" c_casadi__DM__size1
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__size1
  :: DM -> IO Int
casadi__DM__size1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__size1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_size1 :: DMClass a => a -> IO Int
dm_size1 x = casadi__DM__size1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__size2" c_casadi__DM__size2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO CLLong

casadi__DM__size2
  :: DM -> IO Int
casadi__DM__size2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__size2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_size2 :: DMClass a => a -> IO Int
dm_size2 x = casadi__DM__size2 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sparsity" c_casadi__DM__sparsity
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr Sparsity')

casadi__DM__sparsity
  :: DM -> IO Sparsity
casadi__DM__sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_sparsity :: DMClass a => a -> IO Sparsity
dm_sparsity x = casadi__DM__sparsity (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__0" c_casadi__DM__sym__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> CLLong -> CLLong -> CLLong -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi__DM__sym__0
  :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector DM))
casadi__DM__sym__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__0 errStrPtrP x0' x1' x2' x3' x4'
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
dm_sym__0 :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector DM))
dm_sym__0 = casadi__DM__sym__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__1" c_casadi__DM__sym__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CLLong -> CLLong -> IO (Ptr (StdVec (Ptr (StdVec (Ptr DM')))))

casadi__DM__sym__1
  :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector DM))
casadi__DM__sym__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
dm_sym__1 :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector DM))
dm_sym__1 = casadi__DM__sym__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__2" c_casadi__DM__sym__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> CLLong -> CLLong -> IO (Ptr (StdVec (Ptr DM')))

casadi__DM__sym__2
  :: String -> Int -> Int -> Int -> IO (Vector DM)
casadi__DM__sym__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
dm_sym__2 :: String -> Int -> Int -> Int -> IO (Vector DM)
dm_sym__2 = casadi__DM__sym__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__3" c_casadi__DM__sym__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CLLong -> IO (Ptr (StdVec (Ptr DM')))

casadi__DM__sym__3
  :: String -> Sparsity -> Int -> IO (Vector DM)
casadi__DM__sym__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_sym__3 :: String -> Sparsity -> Int -> IO (Vector DM)
dm_sym__3 = casadi__DM__sym__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__4" c_casadi__DM__sym__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__sym__4
  :: String -> Sparsity -> IO DM
casadi__DM__sym__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_sym__4 :: String -> Sparsity -> IO DM
dm_sym__4 = casadi__DM__sym__4


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__5" c_casadi__DM__sym__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__sym__5
  :: String -> (Int, Int) -> IO DM
casadi__DM__sym__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_sym__5 :: String -> (Int, Int) -> IO DM
dm_sym__5 = casadi__DM__sym__5


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__6" c_casadi__DM__sym__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr DM')

casadi__DM__sym__6
  :: String -> IO DM
casadi__DM__sym__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_sym__6 :: String -> IO DM
dm_sym__6 = casadi__DM__sym__6


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__7" c_casadi__DM__sym__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> IO (Ptr DM')

casadi__DM__sym__7
  :: String -> Int -> IO DM
casadi__DM__sym__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_sym__7 :: String -> Int -> IO DM
dm_sym__7 = casadi__DM__sym__7


-- direct wrapper
foreign import ccall unsafe "casadi__DM__sym__8" c_casadi__DM__sym__8
  :: Ptr (Ptr StdString) -> Ptr StdString -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__sym__8
  :: String -> Int -> Int -> IO DM
casadi__DM__sym__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__sym__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_sym__8 :: String -> Int -> Int -> IO DM
dm_sym__8 = casadi__DM__sym__8


-- direct wrapper
foreign import ccall unsafe "casadi__DM__to_file__0" c_casadi__DM__to_file__0
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr StdString -> IO ()

casadi__DM__to_file__0
  :: DM -> String -> IO ()
casadi__DM__to_file__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__to_file__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
dm_to_file__0 :: DMClass a => a -> String -> IO ()
dm_to_file__0 x = casadi__DM__to_file__0 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__to_file__1" c_casadi__DM__to_file__1
  :: Ptr (Ptr StdString) -> Ptr DM' -> Ptr StdString -> Ptr StdString -> IO ()

casadi__DM__to_file__1
  :: DM -> String -> String -> IO ()
casadi__DM__to_file__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__to_file__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
dm_to_file__1 :: DMClass a => a -> String -> String -> IO ()
dm_to_file__1 x = casadi__DM__to_file__1 (castDM x)


-- direct wrapper
foreign import ccall unsafe "casadi__DM__triplet__0" c_casadi__DM__triplet__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> Ptr DM' -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__triplet__0
  :: Vector Int -> Vector Int -> DM -> (Int, Int) -> IO DM
casadi__DM__triplet__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__triplet__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
dm_triplet__0 :: Vector Int -> Vector Int -> DM -> (Int, Int) -> IO DM
dm_triplet__0 = casadi__DM__triplet__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__triplet__1" c_casadi__DM__triplet__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> Ptr DM' -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__triplet__1
  :: Vector Int -> Vector Int -> DM -> Int -> Int -> IO DM
casadi__DM__triplet__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__triplet__1 errStrPtrP x0' x1' x2' x3' x4'
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
dm_triplet__1 :: Vector Int -> Vector Int -> DM -> Int -> Int -> IO DM
dm_triplet__1 = casadi__DM__triplet__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__triplet__2" c_casadi__DM__triplet__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CLLong) -> Ptr (StdVec CLLong) -> Ptr DM' -> IO (Ptr DM')

casadi__DM__triplet__2
  :: Vector Int -> Vector Int -> DM -> IO DM
casadi__DM__triplet__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__triplet__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
dm_triplet__2 :: Vector Int -> Vector Int -> DM -> IO DM
dm_triplet__2 = casadi__DM__triplet__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__type_name" c_casadi__DM__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__DM__type_name
  :: IO String
casadi__DM__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_type_name :: IO String
dm_type_name = casadi__DM__type_name


-- direct wrapper
foreign import ccall unsafe "casadi__DM__unary" c_casadi__DM__unary
  :: Ptr (Ptr StdString) -> CLLong -> Ptr DM' -> IO (Ptr DM')

casadi__DM__unary
  :: Int -> DM -> IO DM
casadi__DM__unary x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__unary errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_unary :: Int -> DM -> IO DM
dm_unary = casadi__DM__unary


-- direct wrapper
foreign import ccall unsafe "casadi__DM__zeros__0" c_casadi__DM__zeros__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CLLong CLLong) -> IO (Ptr DM')

casadi__DM__zeros__0
  :: (Int, Int) -> IO DM
casadi__DM__zeros__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__zeros__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_zeros__0 :: (Int, Int) -> IO DM
dm_zeros__0 = casadi__DM__zeros__0


-- direct wrapper
foreign import ccall unsafe "casadi__DM__zeros__1" c_casadi__DM__zeros__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr DM')

casadi__DM__zeros__1
  :: Sparsity -> IO DM
casadi__DM__zeros__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__zeros__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_zeros__1 :: Sparsity -> IO DM
dm_zeros__1 = casadi__DM__zeros__1


-- direct wrapper
foreign import ccall unsafe "casadi__DM__zeros__2" c_casadi__DM__zeros__2
  :: Ptr (Ptr StdString) -> IO (Ptr DM')

casadi__DM__zeros__2
  :: IO DM
casadi__DM__zeros__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__zeros__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
dm_zeros__2 :: IO DM
dm_zeros__2 = casadi__DM__zeros__2


-- direct wrapper
foreign import ccall unsafe "casadi__DM__zeros__3" c_casadi__DM__zeros__3
  :: Ptr (Ptr StdString) -> CLLong -> IO (Ptr DM')

casadi__DM__zeros__3
  :: Int -> IO DM
casadi__DM__zeros__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__zeros__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
dm_zeros__3 :: Int -> IO DM
dm_zeros__3 = casadi__DM__zeros__3


-- direct wrapper
foreign import ccall unsafe "casadi__DM__zeros__4" c_casadi__DM__zeros__4
  :: Ptr (Ptr StdString) -> CLLong -> CLLong -> IO (Ptr DM')

casadi__DM__zeros__4
  :: Int -> Int -> IO DM
casadi__DM__zeros__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__DM__zeros__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
dm_zeros__4 :: Int -> Int -> IO DM
dm_zeros__4 = casadi__DM__zeros__4

