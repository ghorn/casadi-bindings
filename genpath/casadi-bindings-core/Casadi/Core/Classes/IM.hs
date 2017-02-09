{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.IM
       (
         IM,
         IMClass(..),
         im_T,
         im__0,
         im__1,
         im__10,
         im__2,
         im__3,
         im__4,
         im__5,
         im__6,
         im__7,
         im__8,
         im__9,
         im___nonzero__,
         im_binary,
         im_clear,
         im_colind,
         im_dep__0,
         im_dep__1,
         im_dim,
         im_element_hash,
         im_enlarge__0,
         im_enlarge__1,
         im_erase__0,
         im_erase__1,
         im_erase__2,
         im_erase__3,
         im_eye,
         im_getDescription,
         im_getRepresentation,
         im_get__0,
         im_get__1,
         im_get__2,
         im_get__3,
         im_get__4,
         im_get__5,
         im_get__6,
         im_get_colind,
         im_get_free,
         im_get_input,
         im_get_max_depth,
         im_get_nonzeros,
         im_get_nz__0,
         im_get_nz__1,
         im_get_row,
         im_get_sparsity,
         im_has_duplicates,
         im_has_nz,
         im_has_zeros,
         im_inf__0,
         im_inf__1,
         im_inf__2,
         im_inf__3,
         im_inf__4,
         im_is_column,
         im_is_commutative,
         im_is_constant,
         im_is_dense,
         im_is_empty__0,
         im_is_empty__1,
         im_is_identity,
         im_is_integer,
         im_is_leaf,
         im_is_minus_one,
         im_is_one,
         im_is_regular,
         im_is_row,
         im_is_scalar__0,
         im_is_scalar__1,
         im_is_smooth,
         im_is_square,
         im_is_symbolic,
         im_is_tril,
         im_is_triu,
         im_is_valid_input,
         im_is_vector,
         im_is_zero,
         im_matrix_matrix,
         im_matrix_scalar,
         im_n_dep,
         im_name,
         im_nan__0,
         im_nan__1,
         im_nan__2,
         im_nan__3,
         im_nan__4,
         im_nnz,
         im_nnz_diag,
         im_nnz_lower,
         im_nnz_upper,
         im_numel,
         im_ones__0,
         im_ones__1,
         im_ones__2,
         im_ones__3,
         im_ones__4,
         im_operator__minus,
         im_operator__plus,
         im_operator_double,
         im_operator_int,
         im_print_dense,
         im_print_scalar,
         im_print_sparse,
         im_print_split,
         im_print_vector,
         im_printme,
         im_remove,
         im_reserve__0,
         im_reserve__1,
         im_reset_input,
         im_resize,
         im_row,
         im_sanity_check__0,
         im_sanity_check__1,
         im_scalar_matrix,
         im_setPrecision,
         im_setScientific,
         im_setWidth,
         im_set__0,
         im_set__1,
         im_set__2,
         im_set__3,
         im_set__4,
         im_set__5,
         im_set__6,
         im_set_max_depth__0,
         im_set_max_depth__1,
         im_set_nz__0,
         im_set_nz__1,
         im_size1,
         im_size2,
         im_size__0,
         im_size__1,
         im_sparsity,
         im_sym__0,
         im_sym__1,
         im_sym__2,
         im_sym__3,
         im_sym__4,
         im_sym__5,
         im_sym__6,
         im_sym__7,
         im_sym__8,
         im_triplet__0,
         im_triplet__1,
         im_triplet__2,
         im_type_name,
         im_unary,
         im_zeros__0,
         im_zeros__1,
         im_zeros__2,
         im_zeros__3,
         im_zeros__4,
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
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__0" c_casadi__IM__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr SX' -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__0
  :: SX -> IO IM
casadi__IM__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__0 :: SX -> IO IM
im__0 = casadi__IM__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__1" c_casadi__IM__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr SXElem')) -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__1
  :: Vector SXElem -> IO IM
casadi__IM__CONSTRUCTOR__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__1 :: Vector SXElem -> IO IM
im__1 = casadi__IM__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__2" c_casadi__IM__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> Ptr DM' -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__2
  :: DM -> IO IM
casadi__IM__CONSTRUCTOR__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__2 :: DM -> IO IM
im__2 = casadi__IM__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__3" c_casadi__IM__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> Ptr (StdVec CDouble) -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__3
  :: Vector Double -> IO IM
casadi__IM__CONSTRUCTOR__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__3 :: Vector Double -> IO IM
im__3 = casadi__IM__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__4" c_casadi__IM__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr (StdVec CDouble))) -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__4
  :: Vector (Vector Double) -> IO IM
casadi__IM__CONSTRUCTOR__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__4 :: Vector (Vector Double) -> IO IM
im__4 = casadi__IM__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__5" c_casadi__IM__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> CDouble -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__5
  :: Double -> IO IM
casadi__IM__CONSTRUCTOR__5 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__5 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__5 :: Double -> IO IM
im__5 = casadi__IM__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__6" c_casadi__IM__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> Ptr IM' -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__6
  :: Sparsity -> IM -> IO IM
casadi__IM__CONSTRUCTOR__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im__6 :: Sparsity -> IM -> IO IM
im__6 = casadi__IM__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__7" c_casadi__IM__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__7
  :: Sparsity -> IO IM
casadi__IM__CONSTRUCTOR__7 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__7 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__7 :: Sparsity -> IO IM
im__7 = casadi__IM__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__8" c_casadi__IM__CONSTRUCTOR__8
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__8
  :: Int -> Int -> IO IM
casadi__IM__CONSTRUCTOR__8 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__8 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im__8 :: Int -> Int -> IO IM
im__8 = casadi__IM__CONSTRUCTOR__8


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__9" c_casadi__IM__CONSTRUCTOR__9
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__9
  :: IM -> IO IM
casadi__IM__CONSTRUCTOR__9 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__9 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im__9 :: IM -> IO IM
im__9 = casadi__IM__CONSTRUCTOR__9


-- direct wrapper
foreign import ccall unsafe "casadi__IM__CONSTRUCTOR__10" c_casadi__IM__CONSTRUCTOR__10
  :: Ptr (Ptr StdString) -> IO (Ptr IM')

casadi__IM__CONSTRUCTOR__10
  :: IO IM
casadi__IM__CONSTRUCTOR__10  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__CONSTRUCTOR__10 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im__10 :: IO IM
im__10 = casadi__IM__CONSTRUCTOR__10


-- direct wrapper
foreign import ccall unsafe "casadi__IM__T" c_casadi__IM__T
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi__IM__T
  :: IM -> IO IM
casadi__IM__T x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__T errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_T :: IMClass a => a -> IO IM
im_T x = casadi__IM__T (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM____nonzero__" c_casadi__IM____nonzero__
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM____nonzero__
  :: IM -> IO Bool
casadi__IM____nonzero__ x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM____nonzero__ errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im___nonzero__ :: IMClass a => a -> IO Bool
im___nonzero__ x = casadi__IM____nonzero__ (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__binary" c_casadi__IM__binary
  :: Ptr (Ptr StdString) -> CInt -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi__IM__binary
  :: Int -> IM -> IM -> IO IM
casadi__IM__binary x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__binary errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_binary :: Int -> IM -> IM -> IO IM
im_binary = casadi__IM__binary


-- direct wrapper
foreign import ccall unsafe "casadi__IM__clear" c_casadi__IM__clear
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__clear
  :: IM -> IO ()
casadi__IM__clear x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__clear errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_clear :: IMClass a => a -> IO ()
im_clear x = casadi__IM__clear (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__colind" c_casadi__IM__colind
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO CInt

casadi__IM__colind
  :: IM -> Int -> IO Int
casadi__IM__colind x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__colind errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_colind :: IMClass a => a -> Int -> IO Int
im_colind x = casadi__IM__colind (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__dep__0" c_casadi__IM__dep__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi__IM__dep__0
  :: IM -> IO IM
casadi__IM__dep__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__dep__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_dep__0 :: IMClass a => a -> IO IM
im_dep__0 x = casadi__IM__dep__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__dep__1" c_casadi__IM__dep__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO (Ptr IM')

casadi__IM__dep__1
  :: IM -> Int -> IO IM
casadi__IM__dep__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__dep__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_dep__1 :: IMClass a => a -> Int -> IO IM
im_dep__1 x = casadi__IM__dep__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__dim" c_casadi__IM__dim
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr StdString)

casadi__IM__dim
  :: IM -> IO String
casadi__IM__dim x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__dim errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_dim :: IMClass a => a -> IO String
im_dim x = casadi__IM__dim (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__element_hash" c_casadi__IM__element_hash
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CSize

casadi__IM__element_hash
  :: IM -> IO CSize
casadi__IM__element_hash x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__element_hash errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_element_hash :: IMClass a => a -> IO CSize
im_element_hash x = casadi__IM__element_hash (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__enlarge__0" c_casadi__IM__enlarge__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO ()

casadi__IM__enlarge__0
  :: IM -> Int -> Int -> Vector Int -> Vector Int -> IO ()
casadi__IM__enlarge__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__enlarge__0 errStrPtrP x0' x1' x2' x3' x4'
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
im_enlarge__0 :: IMClass a => a -> Int -> Int -> Vector Int -> Vector Int -> IO ()
im_enlarge__0 x = casadi__IM__enlarge__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__enlarge__1" c_casadi__IM__enlarge__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__IM__enlarge__1
  :: IM -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__IM__enlarge__1 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__enlarge__1 errStrPtrP x0' x1' x2' x3' x4' x5'
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
im_enlarge__1 :: IMClass a => a -> Int -> Int -> Vector Int -> Vector Int -> Bool -> IO ()
im_enlarge__1 x = casadi__IM__enlarge__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__erase__0" c_casadi__IM__erase__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> IO ()

casadi__IM__erase__0
  :: IM -> Vector Int -> IO ()
casadi__IM__erase__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__erase__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
im_erase__0 :: IMClass a => a -> Vector Int -> IO ()
im_erase__0 x = casadi__IM__erase__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__erase__1" c_casadi__IM__erase__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__IM__erase__1
  :: IM -> Vector Int -> Bool -> IO ()
casadi__IM__erase__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__erase__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
im_erase__1 :: IMClass a => a -> Vector Int -> Bool -> IO ()
im_erase__1 x = casadi__IM__erase__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__erase__2" c_casadi__IM__erase__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO ()

casadi__IM__erase__2
  :: IM -> Vector Int -> Vector Int -> IO ()
casadi__IM__erase__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__erase__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
im_erase__2 :: IMClass a => a -> Vector Int -> Vector Int -> IO ()
im_erase__2 x = casadi__IM__erase__2 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__erase__3" c_casadi__IM__erase__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> CInt -> IO ()

casadi__IM__erase__3
  :: IM -> Vector Int -> Vector Int -> Bool -> IO ()
casadi__IM__erase__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__erase__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
im_erase__3 :: IMClass a => a -> Vector Int -> Vector Int -> Bool -> IO ()
im_erase__3 x = casadi__IM__erase__3 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__eye" c_casadi__IM__eye
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr IM')

casadi__IM__eye
  :: Int -> IO IM
casadi__IM__eye x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__eye errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_eye :: Int -> IO IM
im_eye = casadi__IM__eye


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__0" c_casadi__IM__get__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__IM__get__0
  :: IM -> Bool -> IM -> IM -> IO (IM)
casadi__IM__get__0 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__0 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__0/c_casadi__IM__get__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
im_get__0 :: IMClass a => a -> Bool -> IM -> IM -> IO (IM)
im_get__0 x = casadi__IM__get__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__1" c_casadi__IM__get__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__IM__get__1
  :: IM -> Bool -> IM -> Slice -> IO (IM)
casadi__IM__get__1 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__1 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__1/c_casadi__IM__get__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
im_get__1 :: IMClass a => a -> Bool -> IM -> Slice -> IO (IM)
im_get__1 x = casadi__IM__get__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__2" c_casadi__IM__get__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__IM__get__2
  :: IM -> Bool -> Slice -> IM -> IO (IM)
casadi__IM__get__2 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__2 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__2/c_casadi__IM__get__2" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
im_get__2 :: IMClass a => a -> Bool -> Slice -> IM -> IO (IM)
im_get__2 x = casadi__IM__get__2 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__3" c_casadi__IM__get__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__IM__get__3
  :: IM -> Bool -> Slice -> Slice -> IO (IM)
casadi__IM__get__3 x0 x2 x3 x4 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__3 errStrPtrP x0' o1' x2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__3/c_casadi__IM__get__3" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o1''')



-- classy wrapper
im_get__3 :: IMClass a => a -> Bool -> Slice -> Slice -> IO (IM)
im_get__3 x = casadi__IM__get__3 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__4" c_casadi__IM__get__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr Sparsity' -> IO ()

casadi__IM__get__4
  :: IM -> Bool -> Sparsity -> IO (IM)
casadi__IM__get__4 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__4 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__4/c_casadi__IM__get__4" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
im_get__4 :: IMClass a => a -> Bool -> Sparsity -> IO (IM)
im_get__4 x = casadi__IM__get__4 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__5" c_casadi__IM__get__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr IM' -> IO ()

casadi__IM__get__5
  :: IM -> Bool -> IM -> IO (IM)
casadi__IM__get__5 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__5 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__5/c_casadi__IM__get__5" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
im_get__5 :: IMClass a => a -> Bool -> IM -> IO (IM)
im_get__5 x = casadi__IM__get__5 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get__6" c_casadi__IM__get__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr Slice' -> IO ()

casadi__IM__get__6
  :: IM -> Bool -> Slice -> IO (IM)
casadi__IM__get__6 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get__6 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get__6/c_casadi__IM__get__6" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
im_get__6 :: IMClass a => a -> Bool -> Slice -> IO (IM)
im_get__6 x = casadi__IM__get__6 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_colind" c_casadi__IM__get_colind
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec CInt))

casadi__IM__get_colind
  :: IM -> IO (Vector Int)
casadi__IM__get_colind x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_colind errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_get_colind :: IMClass a => a -> IO (Vector Int)
im_get_colind x = casadi__IM__get_colind (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_free" c_casadi__IM__get_free
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr IM')))

casadi__IM__get_free
  :: Function -> IO (Vector IM)
casadi__IM__get_free x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_free errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_get_free :: Function -> IO (Vector IM)
im_get_free = casadi__IM__get_free


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_input" c_casadi__IM__get_input
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr IM')))

casadi__IM__get_input
  :: Function -> IO (Vector IM)
casadi__IM__get_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_get_input :: Function -> IO (Vector IM)
im_get_input = casadi__IM__get_input


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_max_depth" c_casadi__IM__get_max_depth
  :: Ptr (Ptr StdString) -> IO CInt

casadi__IM__get_max_depth
  :: IO Int
casadi__IM__get_max_depth  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_max_depth errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im_get_max_depth :: IO Int
im_get_max_depth = casadi__IM__get_max_depth


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_nonzeros" c_casadi__IM__get_nonzeros
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec CInt))

casadi__IM__get_nonzeros
  :: IM -> IO (Vector Int)
casadi__IM__get_nonzeros x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_nonzeros errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_get_nonzeros :: IMClass a => a -> IO (Vector Int)
im_get_nonzeros x = casadi__IM__get_nonzeros (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_nz__0" c_casadi__IM__get_nz__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr IM' -> IO ()

casadi__IM__get_nz__0
  :: IM -> Bool -> IM -> IO (IM)
casadi__IM__get_nz__0 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_nz__0 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get_nz__0/c_casadi__IM__get_nz__0" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
im_get_nz__0 :: IMClass a => a -> Bool -> IM -> IO (IM)
im_get_nz__0 x = casadi__IM__get_nz__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_nz__1" c_casadi__IM__get_nz__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr IM') -> CInt -> Ptr Slice' -> IO ()

casadi__IM__get_nz__1
  :: IM -> Bool -> Slice -> IO (IM)
casadi__IM__get_nz__1 x0 x2 x3 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_nz__1 errStrPtrP x0' o1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__get_nz__1/c_casadi__IM__get_nz__1" else wrapReturn o1''
  marshalFree x2 x2'
  marshalFree x3 x3'

  return (o1''')



-- classy wrapper
im_get_nz__1 :: IMClass a => a -> Bool -> Slice -> IO (IM)
im_get_nz__1 x = casadi__IM__get_nz__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_row" c_casadi__IM__get_row
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdVec CInt))

casadi__IM__get_row
  :: IM -> IO (Vector Int)
casadi__IM__get_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_get_row :: IMClass a => a -> IO (Vector Int)
im_get_row x = casadi__IM__get_row (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__get_sparsity" c_casadi__IM__get_sparsity
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr Sparsity')

casadi__IM__get_sparsity
  :: IM -> IO Sparsity
casadi__IM__get_sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__get_sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_get_sparsity :: IMClass a => a -> IO Sparsity
im_get_sparsity x = casadi__IM__get_sparsity (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__has_duplicates" c_casadi__IM__has_duplicates
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__has_duplicates
  :: IM -> IO Bool
casadi__IM__has_duplicates x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__has_duplicates errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_has_duplicates :: IMClass a => a -> IO Bool
im_has_duplicates x = casadi__IM__has_duplicates (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__has_nz" c_casadi__IM__has_nz
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO CInt

casadi__IM__has_nz
  :: IM -> Int -> Int -> IO Bool
casadi__IM__has_nz x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__has_nz errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_has_nz :: IMClass a => a -> Int -> Int -> IO Bool
im_has_nz x = casadi__IM__has_nz (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__has_zeros" c_casadi__IM__has_zeros
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__has_zeros
  :: IM -> IO Bool
casadi__IM__has_zeros x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__has_zeros errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_has_zeros :: IMClass a => a -> IO Bool
im_has_zeros x = casadi__IM__has_zeros (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__inf__0" c_casadi__IM__inf__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi__IM__inf__0
  :: (Int, Int) -> IO IM
casadi__IM__inf__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__inf__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_inf__0 :: (Int, Int) -> IO IM
im_inf__0 = casadi__IM__inf__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__inf__1" c_casadi__IM__inf__1
  :: Ptr (Ptr StdString) -> IO (Ptr IM')

casadi__IM__inf__1
  :: IO IM
casadi__IM__inf__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__inf__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im_inf__1 :: IO IM
im_inf__1 = casadi__IM__inf__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__inf__2" c_casadi__IM__inf__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr IM')

casadi__IM__inf__2
  :: Int -> IO IM
casadi__IM__inf__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__inf__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_inf__2 :: Int -> IO IM
im_inf__2 = casadi__IM__inf__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__inf__3" c_casadi__IM__inf__3
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__inf__3
  :: Int -> Int -> IO IM
casadi__IM__inf__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__inf__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_inf__3 :: Int -> Int -> IO IM
im_inf__3 = casadi__IM__inf__3


-- direct wrapper
foreign import ccall unsafe "casadi__IM__inf__4" c_casadi__IM__inf__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr IM')

casadi__IM__inf__4
  :: Sparsity -> IO IM
casadi__IM__inf__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__inf__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_inf__4 :: Sparsity -> IO IM
im_inf__4 = casadi__IM__inf__4


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_column" c_casadi__IM__is_column
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_column
  :: IM -> IO Bool
casadi__IM__is_column x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_column errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_column :: IMClass a => a -> IO Bool
im_is_column x = casadi__IM__is_column (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_commutative" c_casadi__IM__is_commutative
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_commutative
  :: IM -> IO Bool
casadi__IM__is_commutative x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_commutative errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_commutative :: IMClass a => a -> IO Bool
im_is_commutative x = casadi__IM__is_commutative (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_constant" c_casadi__IM__is_constant
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_constant
  :: IM -> IO Bool
casadi__IM__is_constant x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_constant errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_constant :: IMClass a => a -> IO Bool
im_is_constant x = casadi__IM__is_constant (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_dense" c_casadi__IM__is_dense
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_dense
  :: IM -> IO Bool
casadi__IM__is_dense x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_dense errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_dense :: IMClass a => a -> IO Bool
im_is_dense x = casadi__IM__is_dense (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_empty__0" c_casadi__IM__is_empty__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_empty__0
  :: IM -> IO Bool
casadi__IM__is_empty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_empty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_empty__0 :: IMClass a => a -> IO Bool
im_is_empty__0 x = casadi__IM__is_empty__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_empty__1" c_casadi__IM__is_empty__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO CInt

casadi__IM__is_empty__1
  :: IM -> Bool -> IO Bool
casadi__IM__is_empty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_empty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_is_empty__1 :: IMClass a => a -> Bool -> IO Bool
im_is_empty__1 x = casadi__IM__is_empty__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_identity" c_casadi__IM__is_identity
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_identity
  :: IM -> IO Bool
casadi__IM__is_identity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_identity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_identity :: IMClass a => a -> IO Bool
im_is_identity x = casadi__IM__is_identity (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_integer" c_casadi__IM__is_integer
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_integer
  :: IM -> IO Bool
casadi__IM__is_integer x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_integer errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_integer :: IMClass a => a -> IO Bool
im_is_integer x = casadi__IM__is_integer (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_leaf" c_casadi__IM__is_leaf
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_leaf
  :: IM -> IO Bool
casadi__IM__is_leaf x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_leaf errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_leaf :: IMClass a => a -> IO Bool
im_is_leaf x = casadi__IM__is_leaf (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_minus_one" c_casadi__IM__is_minus_one
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_minus_one
  :: IM -> IO Bool
casadi__IM__is_minus_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_minus_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_minus_one :: IMClass a => a -> IO Bool
im_is_minus_one x = casadi__IM__is_minus_one (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_one" c_casadi__IM__is_one
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_one
  :: IM -> IO Bool
casadi__IM__is_one x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_one errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_one :: IMClass a => a -> IO Bool
im_is_one x = casadi__IM__is_one (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_regular" c_casadi__IM__is_regular
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_regular
  :: IM -> IO Bool
casadi__IM__is_regular x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_regular errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_regular :: IMClass a => a -> IO Bool
im_is_regular x = casadi__IM__is_regular (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_row" c_casadi__IM__is_row
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_row
  :: IM -> IO Bool
casadi__IM__is_row x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_row errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_row :: IMClass a => a -> IO Bool
im_is_row x = casadi__IM__is_row (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_scalar__0" c_casadi__IM__is_scalar__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_scalar__0
  :: IM -> IO Bool
casadi__IM__is_scalar__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_scalar__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_scalar__0 :: IMClass a => a -> IO Bool
im_is_scalar__0 x = casadi__IM__is_scalar__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_scalar__1" c_casadi__IM__is_scalar__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO CInt

casadi__IM__is_scalar__1
  :: IM -> Bool -> IO Bool
casadi__IM__is_scalar__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_scalar__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_is_scalar__1 :: IMClass a => a -> Bool -> IO Bool
im_is_scalar__1 x = casadi__IM__is_scalar__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_smooth" c_casadi__IM__is_smooth
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_smooth
  :: IM -> IO Bool
casadi__IM__is_smooth x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_smooth errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_smooth :: IMClass a => a -> IO Bool
im_is_smooth x = casadi__IM__is_smooth (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_square" c_casadi__IM__is_square
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_square
  :: IM -> IO Bool
casadi__IM__is_square x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_square errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_square :: IMClass a => a -> IO Bool
im_is_square x = casadi__IM__is_square (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_symbolic" c_casadi__IM__is_symbolic
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_symbolic
  :: IM -> IO Bool
casadi__IM__is_symbolic x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_symbolic errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_symbolic :: IMClass a => a -> IO Bool
im_is_symbolic x = casadi__IM__is_symbolic (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_tril" c_casadi__IM__is_tril
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_tril
  :: IM -> IO Bool
casadi__IM__is_tril x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_tril errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_tril :: IMClass a => a -> IO Bool
im_is_tril x = casadi__IM__is_tril (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_triu" c_casadi__IM__is_triu
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_triu
  :: IM -> IO Bool
casadi__IM__is_triu x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_triu errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_triu :: IMClass a => a -> IO Bool
im_is_triu x = casadi__IM__is_triu (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_valid_input" c_casadi__IM__is_valid_input
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_valid_input
  :: IM -> IO Bool
casadi__IM__is_valid_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_valid_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_valid_input :: IMClass a => a -> IO Bool
im_is_valid_input x = casadi__IM__is_valid_input (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_vector" c_casadi__IM__is_vector
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_vector
  :: IM -> IO Bool
casadi__IM__is_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_vector :: IMClass a => a -> IO Bool
im_is_vector x = casadi__IM__is_vector (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__is_zero" c_casadi__IM__is_zero
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__is_zero
  :: IM -> IO Bool
casadi__IM__is_zero x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__is_zero errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_is_zero :: IMClass a => a -> IO Bool
im_is_zero x = casadi__IM__is_zero (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__matrix_matrix" c_casadi__IM__matrix_matrix
  :: Ptr (Ptr StdString) -> CInt -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi__IM__matrix_matrix
  :: Int -> IM -> IM -> IO IM
casadi__IM__matrix_matrix x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__matrix_matrix errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_matrix_matrix :: Int -> IM -> IM -> IO IM
im_matrix_matrix = casadi__IM__matrix_matrix


-- direct wrapper
foreign import ccall unsafe "casadi__IM__matrix_scalar" c_casadi__IM__matrix_scalar
  :: Ptr (Ptr StdString) -> CInt -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi__IM__matrix_scalar
  :: Int -> IM -> IM -> IO IM
casadi__IM__matrix_scalar x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__matrix_scalar errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_matrix_scalar :: Int -> IM -> IM -> IO IM
im_matrix_scalar = casadi__IM__matrix_scalar


-- direct wrapper
foreign import ccall unsafe "casadi__IM__n_dep" c_casadi__IM__n_dep
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__n_dep
  :: IM -> IO Int
casadi__IM__n_dep x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__n_dep errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_n_dep :: IMClass a => a -> IO Int
im_n_dep x = casadi__IM__n_dep (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__name" c_casadi__IM__name
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr StdString)

casadi__IM__name
  :: IM -> IO String
casadi__IM__name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_name :: IMClass a => a -> IO String
im_name x = casadi__IM__name (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nan__0" c_casadi__IM__nan__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi__IM__nan__0
  :: (Int, Int) -> IO IM
casadi__IM__nan__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nan__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nan__0 :: (Int, Int) -> IO IM
im_nan__0 = casadi__IM__nan__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nan__1" c_casadi__IM__nan__1
  :: Ptr (Ptr StdString) -> IO (Ptr IM')

casadi__IM__nan__1
  :: IO IM
casadi__IM__nan__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nan__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im_nan__1 :: IO IM
im_nan__1 = casadi__IM__nan__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nan__2" c_casadi__IM__nan__2
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr IM')

casadi__IM__nan__2
  :: Int -> IO IM
casadi__IM__nan__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nan__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nan__2 :: Int -> IO IM
im_nan__2 = casadi__IM__nan__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nan__3" c_casadi__IM__nan__3
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__nan__3
  :: Int -> Int -> IO IM
casadi__IM__nan__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nan__3 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_nan__3 :: Int -> Int -> IO IM
im_nan__3 = casadi__IM__nan__3


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nan__4" c_casadi__IM__nan__4
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr IM')

casadi__IM__nan__4
  :: Sparsity -> IO IM
casadi__IM__nan__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nan__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nan__4 :: Sparsity -> IO IM
im_nan__4 = casadi__IM__nan__4


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nnz" c_casadi__IM__nnz
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__nnz
  :: IM -> IO Int
casadi__IM__nnz x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nnz errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nnz :: IMClass a => a -> IO Int
im_nnz x = casadi__IM__nnz (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nnz_diag" c_casadi__IM__nnz_diag
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__nnz_diag
  :: IM -> IO Int
casadi__IM__nnz_diag x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nnz_diag errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nnz_diag :: IMClass a => a -> IO Int
im_nnz_diag x = casadi__IM__nnz_diag (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nnz_lower" c_casadi__IM__nnz_lower
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__nnz_lower
  :: IM -> IO Int
casadi__IM__nnz_lower x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nnz_lower errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nnz_lower :: IMClass a => a -> IO Int
im_nnz_lower x = casadi__IM__nnz_lower (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__nnz_upper" c_casadi__IM__nnz_upper
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__nnz_upper
  :: IM -> IO Int
casadi__IM__nnz_upper x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__nnz_upper errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_nnz_upper :: IMClass a => a -> IO Int
im_nnz_upper x = casadi__IM__nnz_upper (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__numel" c_casadi__IM__numel
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__numel
  :: IM -> IO Int
casadi__IM__numel x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__numel errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_numel :: IMClass a => a -> IO Int
im_numel x = casadi__IM__numel (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__ones__0" c_casadi__IM__ones__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi__IM__ones__0
  :: (Int, Int) -> IO IM
casadi__IM__ones__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__ones__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_ones__0 :: (Int, Int) -> IO IM
im_ones__0 = casadi__IM__ones__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__ones__1" c_casadi__IM__ones__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr IM')

casadi__IM__ones__1
  :: Sparsity -> IO IM
casadi__IM__ones__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__ones__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_ones__1 :: Sparsity -> IO IM
im_ones__1 = casadi__IM__ones__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__ones__2" c_casadi__IM__ones__2
  :: Ptr (Ptr StdString) -> IO (Ptr IM')

casadi__IM__ones__2
  :: IO IM
casadi__IM__ones__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__ones__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im_ones__2 :: IO IM
im_ones__2 = casadi__IM__ones__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__ones__3" c_casadi__IM__ones__3
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr IM')

casadi__IM__ones__3
  :: Int -> IO IM
casadi__IM__ones__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__ones__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_ones__3 :: Int -> IO IM
im_ones__3 = casadi__IM__ones__3


-- direct wrapper
foreign import ccall unsafe "casadi__IM__ones__4" c_casadi__IM__ones__4
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__ones__4
  :: Int -> Int -> IO IM
casadi__IM__ones__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__ones__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_ones__4 :: Int -> Int -> IO IM
im_ones__4 = casadi__IM__ones__4


-- direct wrapper
foreign import ccall unsafe "casadi__IM__operator__plus" c_casadi__IM__operator__plus
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi__IM__operator__plus
  :: IM -> IO IM
casadi__IM__operator__plus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__operator__plus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_operator__plus :: IMClass a => a -> IO IM
im_operator__plus x = casadi__IM__operator__plus (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__operator__minus" c_casadi__IM__operator__minus
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr IM')

casadi__IM__operator__minus
  :: IM -> IO IM
casadi__IM__operator__minus x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__operator__minus errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_operator__minus :: IMClass a => a -> IO IM
im_operator__minus x = casadi__IM__operator__minus (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__operator_double" c_casadi__IM__operator_double
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CDouble

casadi__IM__operator_double
  :: IM -> IO Double
casadi__IM__operator_double x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__operator_double errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_operator_double :: IMClass a => a -> IO Double
im_operator_double x = casadi__IM__operator_double (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__operator_int" c_casadi__IM__operator_int
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__operator_int
  :: IM -> IO Int
casadi__IM__operator_int x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__operator_int errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_operator_int :: IMClass a => a -> IO Int
im_operator_int x = casadi__IM__operator_int (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__print_dense" c_casadi__IM__print_dense
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__print_dense
  :: IM -> IO ()
casadi__IM__print_dense x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__print_dense errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_print_dense :: IMClass a => a -> IO ()
im_print_dense x = casadi__IM__print_dense (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__print_scalar" c_casadi__IM__print_scalar
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__print_scalar
  :: IM -> IO ()
casadi__IM__print_scalar x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__print_scalar errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_print_scalar :: IMClass a => a -> IO ()
im_print_scalar x = casadi__IM__print_scalar (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__print_sparse" c_casadi__IM__print_sparse
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__print_sparse
  :: IM -> IO ()
casadi__IM__print_sparse x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__print_sparse errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_print_sparse :: IMClass a => a -> IO ()
im_print_sparse x = casadi__IM__print_sparse (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__print_split" c_casadi__IM__print_split
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (Ptr (StdVec (Ptr StdString))) -> Ptr (Ptr (StdVec (Ptr StdString))) -> IO ()

casadi__IM__print_split
  :: IM -> IO (Vector String, Vector String)
casadi__IM__print_split x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__print_split errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__IM__print_split/c_casadi__IM__print_split" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__IM__print_split/c_casadi__IM__print_split" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
im_print_split :: IMClass a => a -> IO (Vector String, Vector String)
im_print_split x = casadi__IM__print_split (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__print_vector" c_casadi__IM__print_vector
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__print_vector
  :: IM -> IO ()
casadi__IM__print_vector x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__print_vector errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_print_vector :: IMClass a => a -> IO ()
im_print_vector x = casadi__IM__print_vector (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__printme" c_casadi__IM__printme
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi__IM__printme
  :: IM -> IM -> IO IM
casadi__IM__printme x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__printme errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_printme :: IMClass a => a -> IM -> IO IM
im_printme x = casadi__IM__printme (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__remove" c_casadi__IM__remove
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO ()

casadi__IM__remove
  :: IM -> Vector Int -> Vector Int -> IO ()
casadi__IM__remove x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__remove errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
im_remove :: IMClass a => a -> Vector Int -> Vector Int -> IO ()
im_remove x = casadi__IM__remove (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__reserve__0" c_casadi__IM__reserve__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO ()

casadi__IM__reserve__0
  :: IM -> Int -> Int -> IO ()
casadi__IM__reserve__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__reserve__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
im_reserve__0 :: IMClass a => a -> Int -> Int -> IO ()
im_reserve__0 x = casadi__IM__reserve__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__reserve__1" c_casadi__IM__reserve__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO ()

casadi__IM__reserve__1
  :: IM -> Int -> IO ()
casadi__IM__reserve__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__reserve__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
im_reserve__1 :: IMClass a => a -> Int -> IO ()
im_reserve__1 x = casadi__IM__reserve__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__reset_input" c_casadi__IM__reset_input
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__reset_input
  :: IM -> IO ()
casadi__IM__reset_input x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__reset_input errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_reset_input :: IMClass a => a -> IO ()
im_reset_input x = casadi__IM__reset_input (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__resize" c_casadi__IM__resize
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> CInt -> IO ()

casadi__IM__resize
  :: IM -> Int -> Int -> IO ()
casadi__IM__resize x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__resize errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
im_resize :: IMClass a => a -> Int -> Int -> IO ()
im_resize x = casadi__IM__resize (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__row" c_casadi__IM__row
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO CInt

casadi__IM__row
  :: IM -> Int -> IO Int
casadi__IM__row x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__row errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_row :: IMClass a => a -> Int -> IO Int
im_row x = casadi__IM__row (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sanity_check__0" c_casadi__IM__sanity_check__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO ()

casadi__IM__sanity_check__0
  :: IM -> IO ()
casadi__IM__sanity_check__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sanity_check__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_sanity_check__0 :: IMClass a => a -> IO ()
im_sanity_check__0 x = casadi__IM__sanity_check__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sanity_check__1" c_casadi__IM__sanity_check__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO ()

casadi__IM__sanity_check__1
  :: IM -> Bool -> IO ()
casadi__IM__sanity_check__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sanity_check__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
im_sanity_check__1 :: IMClass a => a -> Bool -> IO ()
im_sanity_check__1 x = casadi__IM__sanity_check__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__scalar_matrix" c_casadi__IM__scalar_matrix
  :: Ptr (Ptr StdString) -> CInt -> Ptr IM' -> Ptr IM' -> IO (Ptr IM')

casadi__IM__scalar_matrix
  :: Int -> IM -> IM -> IO IM
casadi__IM__scalar_matrix x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__scalar_matrix errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_scalar_matrix :: Int -> IM -> IM -> IO IM
im_scalar_matrix = casadi__IM__scalar_matrix


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__0" c_casadi__IM__set__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr IM' -> Ptr IM' -> IO ()

casadi__IM__set__0
  :: IM -> IM -> Bool -> IM -> IM -> IO ()
casadi__IM__set__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__0 errStrPtrP x0' x1' x2' x3' x4'
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
im_set__0 :: IMClass a => a -> IM -> Bool -> IM -> IM -> IO ()
im_set__0 x = casadi__IM__set__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__1" c_casadi__IM__set__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr IM' -> Ptr Slice' -> IO ()

casadi__IM__set__1
  :: IM -> IM -> Bool -> IM -> Slice -> IO ()
casadi__IM__set__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__1 errStrPtrP x0' x1' x2' x3' x4'
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
im_set__1 :: IMClass a => a -> IM -> Bool -> IM -> Slice -> IO ()
im_set__1 x = casadi__IM__set__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__2" c_casadi__IM__set__2
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr Slice' -> Ptr IM' -> IO ()

casadi__IM__set__2
  :: IM -> IM -> Bool -> Slice -> IM -> IO ()
casadi__IM__set__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__2 errStrPtrP x0' x1' x2' x3' x4'
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
im_set__2 :: IMClass a => a -> IM -> Bool -> Slice -> IM -> IO ()
im_set__2 x = casadi__IM__set__2 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__3" c_casadi__IM__set__3
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr Slice' -> Ptr Slice' -> IO ()

casadi__IM__set__3
  :: IM -> IM -> Bool -> Slice -> Slice -> IO ()
casadi__IM__set__3 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__3 errStrPtrP x0' x1' x2' x3' x4'
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
im_set__3 :: IMClass a => a -> IM -> Bool -> Slice -> Slice -> IO ()
im_set__3 x = casadi__IM__set__3 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__4" c_casadi__IM__set__4
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr Sparsity' -> IO ()

casadi__IM__set__4
  :: IM -> IM -> Bool -> Sparsity -> IO ()
casadi__IM__set__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
im_set__4 :: IMClass a => a -> IM -> Bool -> Sparsity -> IO ()
im_set__4 x = casadi__IM__set__4 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__5" c_casadi__IM__set__5
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr IM' -> IO ()

casadi__IM__set__5
  :: IM -> IM -> Bool -> IM -> IO ()
casadi__IM__set__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
im_set__5 :: IMClass a => a -> IM -> Bool -> IM -> IO ()
im_set__5 x = casadi__IM__set__5 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set__6" c_casadi__IM__set__6
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr Slice' -> IO ()

casadi__IM__set__6
  :: IM -> IM -> Bool -> Slice -> IO ()
casadi__IM__set__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
im_set__6 :: IMClass a => a -> IM -> Bool -> Slice -> IO ()
im_set__6 x = casadi__IM__set__6 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__setPrecision" c_casadi__IM__setPrecision
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__IM__setPrecision
  :: Int -> IO ()
casadi__IM__setPrecision x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__setPrecision errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_setPrecision :: Int -> IO ()
im_setPrecision = casadi__IM__setPrecision


-- direct wrapper
foreign import ccall unsafe "casadi__IM__setScientific" c_casadi__IM__setScientific
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__IM__setScientific
  :: Bool -> IO ()
casadi__IM__setScientific x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__setScientific errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_setScientific :: Bool -> IO ()
im_setScientific = casadi__IM__setScientific


-- direct wrapper
foreign import ccall unsafe "casadi__IM__setWidth" c_casadi__IM__setWidth
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__IM__setWidth
  :: Int -> IO ()
casadi__IM__setWidth x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__setWidth errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_setWidth :: Int -> IO ()
im_setWidth = casadi__IM__setWidth


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set_max_depth__0" c_casadi__IM__set_max_depth__0
  :: Ptr (Ptr StdString) -> IO ()

casadi__IM__set_max_depth__0
  :: IO ()
casadi__IM__set_max_depth__0  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set_max_depth__0 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ()



-- classy wrapper
im_set_max_depth__0 :: IO ()
im_set_max_depth__0 = casadi__IM__set_max_depth__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set_max_depth__1" c_casadi__IM__set_max_depth__1
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__IM__set_max_depth__1
  :: Int -> IO ()
casadi__IM__set_max_depth__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set_max_depth__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
im_set_max_depth__1 :: Int -> IO ()
im_set_max_depth__1 = casadi__IM__set_max_depth__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set_nz__0" c_casadi__IM__set_nz__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr IM' -> IO ()

casadi__IM__set_nz__0
  :: IM -> IM -> Bool -> IM -> IO ()
casadi__IM__set_nz__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set_nz__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
im_set_nz__0 :: IMClass a => a -> IM -> Bool -> IM -> IO ()
im_set_nz__0 x = casadi__IM__set_nz__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__set_nz__1" c_casadi__IM__set_nz__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> Ptr IM' -> CInt -> Ptr Slice' -> IO ()

casadi__IM__set_nz__1
  :: IM -> IM -> Bool -> Slice -> IO ()
casadi__IM__set_nz__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__set_nz__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
im_set_nz__1 :: IMClass a => a -> IM -> Bool -> Slice -> IO ()
im_set_nz__1 x = casadi__IM__set_nz__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__size__0" c_casadi__IM__size__0
  :: Ptr (Ptr StdString) -> Ptr IM' -> CInt -> IO CInt

casadi__IM__size__0
  :: IM -> Int -> IO Int
casadi__IM__size__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__size__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_size__0 :: IMClass a => a -> Int -> IO Int
im_size__0 x = casadi__IM__size__0 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__size__1" c_casadi__IM__size__1
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr (StdPair CInt CInt))

casadi__IM__size__1
  :: IM -> IO (Int, Int)
casadi__IM__size__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__size__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_size__1 :: IMClass a => a -> IO (Int, Int)
im_size__1 x = casadi__IM__size__1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__size1" c_casadi__IM__size1
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__size1
  :: IM -> IO Int
casadi__IM__size1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__size1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_size1 :: IMClass a => a -> IO Int
im_size1 x = casadi__IM__size1 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__size2" c_casadi__IM__size2
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO CInt

casadi__IM__size2
  :: IM -> IO Int
casadi__IM__size2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__size2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_size2 :: IMClass a => a -> IO Int
im_size2 x = casadi__IM__size2 (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sparsity" c_casadi__IM__sparsity
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr Sparsity')

casadi__IM__sparsity
  :: IM -> IO Sparsity
casadi__IM__sparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_sparsity :: IMClass a => a -> IO Sparsity
im_sparsity x = casadi__IM__sparsity (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__0" c_casadi__IM__sym__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi__IM__sym__0
  :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector IM))
casadi__IM__sym__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__0 errStrPtrP x0' x1' x2' x3' x4'
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
im_sym__0 :: String -> Int -> Int -> Int -> Int -> IO (Vector (Vector IM))
im_sym__0 = casadi__IM__sym__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__1" c_casadi__IM__sym__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CInt -> CInt -> IO (Ptr (StdVec (Ptr (StdVec (Ptr IM')))))

casadi__IM__sym__1
  :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector IM))
casadi__IM__sym__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
im_sym__1 :: String -> Sparsity -> Int -> Int -> IO (Vector (Vector IM))
im_sym__1 = casadi__IM__sym__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__2" c_casadi__IM__sym__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> CInt -> IO (Ptr (StdVec (Ptr IM')))

casadi__IM__sym__2
  :: String -> Int -> Int -> Int -> IO (Vector IM)
casadi__IM__sym__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
im_sym__2 :: String -> Int -> Int -> Int -> IO (Vector IM)
im_sym__2 = casadi__IM__sym__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__3" c_casadi__IM__sym__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> CInt -> IO (Ptr (StdVec (Ptr IM')))

casadi__IM__sym__3
  :: String -> Sparsity -> Int -> IO (Vector IM)
casadi__IM__sym__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_sym__3 :: String -> Sparsity -> Int -> IO (Vector IM)
im_sym__3 = casadi__IM__sym__3


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__4" c_casadi__IM__sym__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr IM')

casadi__IM__sym__4
  :: String -> Sparsity -> IO IM
casadi__IM__sym__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_sym__4 :: String -> Sparsity -> IO IM
im_sym__4 = casadi__IM__sym__4


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__5" c_casadi__IM__sym__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi__IM__sym__5
  :: String -> (Int, Int) -> IO IM
casadi__IM__sym__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_sym__5 :: String -> (Int, Int) -> IO IM
im_sym__5 = casadi__IM__sym__5


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__6" c_casadi__IM__sym__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr IM')

casadi__IM__sym__6
  :: String -> IO IM
casadi__IM__sym__6 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__6 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_sym__6 :: String -> IO IM
im_sym__6 = casadi__IM__sym__6


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__7" c_casadi__IM__sym__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> IO (Ptr IM')

casadi__IM__sym__7
  :: String -> Int -> IO IM
casadi__IM__sym__7 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__7 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_sym__7 :: String -> Int -> IO IM
im_sym__7 = casadi__IM__sym__7


-- direct wrapper
foreign import ccall unsafe "casadi__IM__sym__8" c_casadi__IM__sym__8
  :: Ptr (Ptr StdString) -> Ptr StdString -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__sym__8
  :: String -> Int -> Int -> IO IM
casadi__IM__sym__8 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__sym__8 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_sym__8 :: String -> Int -> Int -> IO IM
im_sym__8 = casadi__IM__sym__8


-- direct wrapper
foreign import ccall unsafe "casadi__IM__triplet__0" c_casadi__IM__triplet__0
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr IM' -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi__IM__triplet__0
  :: Vector Int -> Vector Int -> IM -> (Int, Int) -> IO IM
casadi__IM__triplet__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__triplet__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
im_triplet__0 :: Vector Int -> Vector Int -> IM -> (Int, Int) -> IO IM
im_triplet__0 = casadi__IM__triplet__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__triplet__1" c_casadi__IM__triplet__1
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr IM' -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__triplet__1
  :: Vector Int -> Vector Int -> IM -> Int -> Int -> IO IM
casadi__IM__triplet__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__triplet__1 errStrPtrP x0' x1' x2' x3' x4'
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
im_triplet__1 :: Vector Int -> Vector Int -> IM -> Int -> Int -> IO IM
im_triplet__1 = casadi__IM__triplet__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__triplet__2" c_casadi__IM__triplet__2
  :: Ptr (Ptr StdString) -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr IM' -> IO (Ptr IM')

casadi__IM__triplet__2
  :: Vector Int -> Vector Int -> IM -> IO IM
casadi__IM__triplet__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__triplet__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
im_triplet__2 :: Vector Int -> Vector Int -> IM -> IO IM
im_triplet__2 = casadi__IM__triplet__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__type_name" c_casadi__IM__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__IM__type_name
  :: IO String
casadi__IM__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im_type_name :: IO String
im_type_name = casadi__IM__type_name


-- direct wrapper
foreign import ccall unsafe "casadi__IM__unary" c_casadi__IM__unary
  :: Ptr (Ptr StdString) -> CInt -> Ptr IM' -> IO (Ptr IM')

casadi__IM__unary
  :: Int -> IM -> IO IM
casadi__IM__unary x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__unary errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_unary :: Int -> IM -> IO IM
im_unary = casadi__IM__unary


-- direct wrapper
foreign import ccall unsafe "casadi__IM__zeros__0" c_casadi__IM__zeros__0
  :: Ptr (Ptr StdString) -> Ptr (StdPair CInt CInt) -> IO (Ptr IM')

casadi__IM__zeros__0
  :: (Int, Int) -> IO IM
casadi__IM__zeros__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__zeros__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_zeros__0 :: (Int, Int) -> IO IM
im_zeros__0 = casadi__IM__zeros__0


-- direct wrapper
foreign import ccall unsafe "casadi__IM__zeros__1" c_casadi__IM__zeros__1
  :: Ptr (Ptr StdString) -> Ptr Sparsity' -> IO (Ptr IM')

casadi__IM__zeros__1
  :: Sparsity -> IO IM
casadi__IM__zeros__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__zeros__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_zeros__1 :: Sparsity -> IO IM
im_zeros__1 = casadi__IM__zeros__1


-- direct wrapper
foreign import ccall unsafe "casadi__IM__zeros__2" c_casadi__IM__zeros__2
  :: Ptr (Ptr StdString) -> IO (Ptr IM')

casadi__IM__zeros__2
  :: IO IM
casadi__IM__zeros__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__zeros__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
im_zeros__2 :: IO IM
im_zeros__2 = casadi__IM__zeros__2


-- direct wrapper
foreign import ccall unsafe "casadi__IM__zeros__3" c_casadi__IM__zeros__3
  :: Ptr (Ptr StdString) -> CInt -> IO (Ptr IM')

casadi__IM__zeros__3
  :: Int -> IO IM
casadi__IM__zeros__3 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__zeros__3 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_zeros__3 :: Int -> IO IM
im_zeros__3 = casadi__IM__zeros__3


-- direct wrapper
foreign import ccall unsafe "casadi__IM__zeros__4" c_casadi__IM__zeros__4
  :: Ptr (Ptr StdString) -> CInt -> CInt -> IO (Ptr IM')

casadi__IM__zeros__4
  :: Int -> Int -> IO IM
casadi__IM__zeros__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__zeros__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
im_zeros__4 :: Int -> Int -> IO IM
im_zeros__4 = casadi__IM__zeros__4


-- direct wrapper
foreign import ccall unsafe "casadi__IM__getRepresentation" c_casadi__IM__getRepresentation
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr StdString)

casadi__IM__getRepresentation
  :: IM -> IO String
casadi__IM__getRepresentation x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__getRepresentation errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_getRepresentation :: IMClass a => a -> IO String
im_getRepresentation x = casadi__IM__getRepresentation (castIM x)


-- direct wrapper
foreign import ccall unsafe "casadi__IM__getDescription" c_casadi__IM__getDescription
  :: Ptr (Ptr StdString) -> Ptr IM' -> IO (Ptr StdString)

casadi__IM__getDescription
  :: IM -> IO String
casadi__IM__getDescription x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__IM__getDescription errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
im_getDescription :: IMClass a => a -> IO String
im_getDescription x = casadi__IM__getDescription (castIM x)

