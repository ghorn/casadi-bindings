{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Function
       (
         Function,
         FunctionClass(..),
         function__0,
         function__1,
         function__10,
         function__11,
         function__12,
         function__13,
         function__2,
         function__3,
         function__4,
         function__5,
         function__6,
         function__7,
         function__8,
         function__9,
         function_assert_size_in,
         function_assert_size_out,
         function_call__0,
         function_call__1,
         function_call__10,
         function_call__11,
         function_call__12,
         function_call__13,
         function_call__14,
         function_call__15,
         function_call__16,
         function_call__17,
         function_call__2,
         function_call__3,
         function_call__4,
         function_call__5,
         function_call__6,
         function_call__7,
         function_call__8,
         function_call__9,
         function_checkInputs,
         function_check_name,
         function_checkout,
         function_conditional__0,
         function_conditional__1,
         function_conic_debug,
         function_default_in,
         function_expand__0,
         function_expand__1,
         function_expand__2,
         function_factory__0,
         function_factory__1,
         function_factory__2,
         function_fix_name,
         function_forward,
         function_free_mx,
         function_free_sx,
         function_fullJacobian,
         function_generate__0,
         function_generate__1,
         function_generate__2,
         function_generate__3,
         function_generate_dependencies__0,
         function_generate_dependencies__1,
         function_generate_lifted,
         function_getAlgorithmSize,
         function_getAtomicInput,
         function_getAtomicInputReal,
         function_getAtomicOperation,
         function_getAtomicOutput,
         function_getWorkSize,
         function_get_function__0,
         function_get_function__1,
         function_has_free,
         function_has_function,
         function_hessian__0,
         function_hessian__1,
         function_hessian__2,
         function_hessian__3,
         function_hessian__4,
         function_hessian__5,
         function_hessian__6,
         function_if_else__0,
         function_if_else__1,
         function_index_in,
         function_index_out,
         function_is_a__0,
         function_is_a__1,
         function_jacobian__0,
         function_jacobian__1,
         function_jacobian__10,
         function_jacobian__11,
         function_jacobian__12,
         function_jacobian__13,
         function_jacobian__14,
         function_jacobian__2,
         function_jacobian__3,
         function_jacobian__4,
         function_jacobian__5,
         function_jacobian__6,
         function_jacobian__7,
         function_jacobian__8,
         function_jacobian__9,
         function_map__0,
         function_map__1,
         function_map__2,
         function_map__3,
         function_map__4,
         function_map__5,
         function_mapaccum__0,
         function_mapaccum__1,
         function_mapaccum__2,
         function_mapaccum__3,
         function_mapaccum__4,
         function_mapaccum__5,
         function_mapaccum__6,
         function_mapsum__0,
         function_mapsum__1,
         function_mx_in__0,
         function_mx_in__1,
         function_mx_in__2,
         function_mx_out__0,
         function_mx_out__1,
         function_mx_out__2,
         function_n_in,
         function_n_nodes,
         function_n_out,
         function_name,
         function_name_in__0,
         function_name_in__1,
         function_name_out__0,
         function_name_out__1,
         function_nnz_in__0,
         function_nnz_in__1,
         function_nnz_in__2,
         function_nnz_out__0,
         function_nnz_out__1,
         function_nnz_out__2,
         function_numel_in__0,
         function_numel_in__1,
         function_numel_in__2,
         function_numel_out__0,
         function_numel_out__1,
         function_numel_out__2,
         function_oracle,
         function_print_dimensions,
         function_print_free,
         function_print_option,
         function_print_options,
         function_release,
         function_reverse,
         function_setFullJacobian,
         function_size1_in__0,
         function_size1_in__1,
         function_size1_out__0,
         function_size1_out__1,
         function_size2_in__0,
         function_size2_in__1,
         function_size2_out__0,
         function_size2_out__1,
         function_size_in__0,
         function_size_in__1,
         function_size_out__0,
         function_size_out__1,
         function_slice__0,
         function_slice__1,
         function_spCanEvaluate,
         function_sparsity_in__0,
         function_sparsity_in__1,
         function_sparsity_jac__0,
         function_sparsity_jac__1,
         function_sparsity_jac__10,
         function_sparsity_jac__11,
         function_sparsity_jac__12,
         function_sparsity_jac__13,
         function_sparsity_jac__14,
         function_sparsity_jac__2,
         function_sparsity_jac__3,
         function_sparsity_jac__4,
         function_sparsity_jac__5,
         function_sparsity_jac__6,
         function_sparsity_jac__7,
         function_sparsity_jac__8,
         function_sparsity_jac__9,
         function_sparsity_out__0,
         function_sparsity_out__1,
         function_stats__0,
         function_stats__1,
         function_sx_in__0,
         function_sx_in__1,
         function_sx_in__2,
         function_sx_out__0,
         function_sx_out__1,
         function_sx_out__2,
         function_sz_arg,
         function_sz_iw,
         function_sz_res,
         function_sz_w,
         function_type_name,
         function_which_depends,
         function_wrap,
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
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__0" c_casadi__Function__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__0
  :: String -> M.Map String MX -> Vector String -> Vector String -> IO Function
casadi__Function__CONSTRUCTOR__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function__0 :: String -> M.Map String MX -> Vector String -> Vector String -> IO Function
function__0 = casadi__Function__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__1" c_casadi__Function__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__1
  :: String -> M.Map String MX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Function__CONSTRUCTOR__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__1 errStrPtrP x0' x1' x2' x3' x4'
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
function__1 :: String -> M.Map String MX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
function__1 = casadi__Function__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__2" c_casadi__Function__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__2
  :: String -> Vector MX -> Vector MX -> Vector String -> Vector String -> IO Function
casadi__Function__CONSTRUCTOR__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__2 errStrPtrP x0' x1' x2' x3' x4'
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
function__2 :: String -> Vector MX -> Vector MX -> Vector String -> Vector String -> IO Function
function__2 = casadi__Function__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__3" c_casadi__Function__CONSTRUCTOR__3
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__3
  :: String -> Vector MX -> Vector MX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Function__CONSTRUCTOR__3 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__3 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function__3 :: String -> Vector MX -> Vector MX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
function__3 = casadi__Function__CONSTRUCTOR__3


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__4" c_casadi__Function__CONSTRUCTOR__4
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__4
  :: String -> Vector MX -> Vector MX -> IO Function
casadi__Function__CONSTRUCTOR__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function__4 :: String -> Vector MX -> Vector MX -> IO Function
function__4 = casadi__Function__CONSTRUCTOR__4


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__5" c_casadi__Function__CONSTRUCTOR__5
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr MX')) -> Ptr (StdVec (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__5
  :: String -> Vector MX -> Vector MX -> M.Map String GenericType -> IO Function
casadi__Function__CONSTRUCTOR__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function__5 :: String -> Vector MX -> Vector MX -> M.Map String GenericType -> IO Function
function__5 = casadi__Function__CONSTRUCTOR__5


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__6" c_casadi__Function__CONSTRUCTOR__6
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__6
  :: String -> M.Map String SX -> Vector String -> Vector String -> IO Function
casadi__Function__CONSTRUCTOR__6 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__6 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function__6 :: String -> M.Map String SX -> Vector String -> Vector String -> IO Function
function__6 = casadi__Function__CONSTRUCTOR__6


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__7" c_casadi__Function__CONSTRUCTOR__7
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__7
  :: String -> M.Map String SX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Function__CONSTRUCTOR__7 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__7 errStrPtrP x0' x1' x2' x3' x4'
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
function__7 :: String -> M.Map String SX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
function__7 = casadi__Function__CONSTRUCTOR__7


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__8" c_casadi__Function__CONSTRUCTOR__8
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__8
  :: String -> Vector SX -> Vector SX -> Vector String -> Vector String -> IO Function
casadi__Function__CONSTRUCTOR__8 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__8 errStrPtrP x0' x1' x2' x3' x4'
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
function__8 :: String -> Vector SX -> Vector SX -> Vector String -> Vector String -> IO Function
function__8 = casadi__Function__CONSTRUCTOR__8


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__9" c_casadi__Function__CONSTRUCTOR__9
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__9
  :: String -> Vector SX -> Vector SX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Function__CONSTRUCTOR__9 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__9 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function__9 :: String -> Vector SX -> Vector SX -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
function__9 = casadi__Function__CONSTRUCTOR__9


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__10" c_casadi__Function__CONSTRUCTOR__10
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__10
  :: String -> Vector SX -> Vector SX -> IO Function
casadi__Function__CONSTRUCTOR__10 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__10 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function__10 :: String -> Vector SX -> Vector SX -> IO Function
function__10 = casadi__Function__CONSTRUCTOR__10


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__11" c_casadi__Function__CONSTRUCTOR__11
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr SX')) -> Ptr (StdVec (Ptr SX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__11
  :: String -> Vector SX -> Vector SX -> M.Map String GenericType -> IO Function
casadi__Function__CONSTRUCTOR__11 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__11 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function__11 :: String -> Vector SX -> Vector SX -> M.Map String GenericType -> IO Function
function__11 = casadi__Function__CONSTRUCTOR__11


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__12" c_casadi__Function__CONSTRUCTOR__12
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__12
  :: String -> IO Function
casadi__Function__CONSTRUCTOR__12 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__12 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function__12 :: String -> IO Function
function__12 = casadi__Function__CONSTRUCTOR__12


-- direct wrapper
foreign import ccall unsafe "casadi__Function__CONSTRUCTOR__13" c_casadi__Function__CONSTRUCTOR__13
  :: Ptr (Ptr StdString) -> IO (Ptr Function')

casadi__Function__CONSTRUCTOR__13
  :: IO Function
casadi__Function__CONSTRUCTOR__13  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__CONSTRUCTOR__13 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
function__13 :: IO Function
function__13 = casadi__Function__CONSTRUCTOR__13


-- direct wrapper
foreign import ccall unsafe "casadi__Function__assert_size_in" c_casadi__Function__assert_size_in
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> CInt -> IO ()

casadi__Function__assert_size_in
  :: Function -> Int -> Int -> Int -> IO ()
casadi__Function__assert_size_in x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__assert_size_in errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
function_assert_size_in :: FunctionClass a => a -> Int -> Int -> Int -> IO ()
function_assert_size_in x = casadi__Function__assert_size_in (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__assert_size_out" c_casadi__Function__assert_size_out
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> CInt -> IO ()

casadi__Function__assert_size_out
  :: Function -> Int -> Int -> Int -> IO ()
casadi__Function__assert_size_out x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__assert_size_out errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
function_assert_size_out :: FunctionClass a => a -> Int -> Int -> Int -> IO ()
function_assert_size_out x = casadi__Function__assert_size_out (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__0" c_casadi__Function__call__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (Ptr (StdMap StdString (Ptr MX'))) -> IO ()

casadi__Function__call__0
  :: Function -> M.Map String MX -> IO (M.Map String MX)
casadi__Function__call__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__0 errStrPtrP x0' x1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__0/c_casadi__Function__call__0" else wrapReturn o2''

  return (o2''')



-- classy wrapper
function_call__0 :: FunctionClass a => a -> M.Map String MX -> IO (M.Map String MX)
function_call__0 x = casadi__Function__call__0 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__1" c_casadi__Function__call__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (Ptr (StdMap StdString (Ptr MX'))) -> CInt -> IO ()

casadi__Function__call__1
  :: Function -> M.Map String MX -> Bool -> IO (M.Map String MX)
casadi__Function__call__1 x0 x1 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__1 errStrPtrP x0' x1' o2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__1/c_casadi__Function__call__1" else wrapReturn o2''
  marshalFree x3 x3'

  return (o2''')



-- classy wrapper
function_call__1 :: FunctionClass a => a -> M.Map String MX -> Bool -> IO (M.Map String MX)
function_call__1 x = casadi__Function__call__1 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__2" c_casadi__Function__call__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr MX')) -> Ptr (Ptr (StdMap StdString (Ptr MX'))) -> CInt -> CInt -> IO ()

casadi__Function__call__2
  :: Function -> M.Map String MX -> Bool -> Bool -> IO (M.Map String MX)
casadi__Function__call__2 x0 x1 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__2 errStrPtrP x0' x1' o2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__2/c_casadi__Function__call__2" else wrapReturn o2''
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o2''')



-- classy wrapper
function_call__2 :: FunctionClass a => a -> M.Map String MX -> Bool -> Bool -> IO (M.Map String MX)
function_call__2 x = casadi__Function__call__2 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__3" c_casadi__Function__call__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (Ptr (StdMap StdString (Ptr SX'))) -> IO ()

casadi__Function__call__3
  :: Function -> M.Map String SX -> IO (M.Map String SX)
casadi__Function__call__3 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__3 errStrPtrP x0' x1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__3/c_casadi__Function__call__3" else wrapReturn o2''

  return (o2''')



-- classy wrapper
function_call__3 :: FunctionClass a => a -> M.Map String SX -> IO (M.Map String SX)
function_call__3 x = casadi__Function__call__3 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__4" c_casadi__Function__call__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (Ptr (StdMap StdString (Ptr SX'))) -> CInt -> IO ()

casadi__Function__call__4
  :: Function -> M.Map String SX -> Bool -> IO (M.Map String SX)
casadi__Function__call__4 x0 x1 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__4 errStrPtrP x0' x1' o2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__4/c_casadi__Function__call__4" else wrapReturn o2''
  marshalFree x3 x3'

  return (o2''')



-- classy wrapper
function_call__4 :: FunctionClass a => a -> M.Map String SX -> Bool -> IO (M.Map String SX)
function_call__4 x = casadi__Function__call__4 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__5" c_casadi__Function__call__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr SX')) -> Ptr (Ptr (StdMap StdString (Ptr SX'))) -> CInt -> CInt -> IO ()

casadi__Function__call__5
  :: Function -> M.Map String SX -> Bool -> Bool -> IO (M.Map String SX)
casadi__Function__call__5 x0 x1 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__5 errStrPtrP x0' x1' o2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__5/c_casadi__Function__call__5" else wrapReturn o2''
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o2''')



-- classy wrapper
function_call__5 :: FunctionClass a => a -> M.Map String SX -> Bool -> Bool -> IO (M.Map String SX)
function_call__5 x = casadi__Function__call__5 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__6" c_casadi__Function__call__6
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr DM')) -> Ptr (Ptr (StdMap StdString (Ptr DM'))) -> IO ()

casadi__Function__call__6
  :: Function -> M.Map String DM -> IO (M.Map String DM)
casadi__Function__call__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__6 errStrPtrP x0' x1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__6/c_casadi__Function__call__6" else wrapReturn o2''

  return (o2''')



-- classy wrapper
function_call__6 :: FunctionClass a => a -> M.Map String DM -> IO (M.Map String DM)
function_call__6 x = casadi__Function__call__6 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__7" c_casadi__Function__call__7
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr DM')) -> Ptr (Ptr (StdMap StdString (Ptr DM'))) -> CInt -> IO ()

casadi__Function__call__7
  :: Function -> M.Map String DM -> Bool -> IO (M.Map String DM)
casadi__Function__call__7 x0 x1 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__7 errStrPtrP x0' x1' o2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__7/c_casadi__Function__call__7" else wrapReturn o2''
  marshalFree x3 x3'

  return (o2''')



-- classy wrapper
function_call__7 :: FunctionClass a => a -> M.Map String DM -> Bool -> IO (M.Map String DM)
function_call__7 x = casadi__Function__call__7 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__8" c_casadi__Function__call__8
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr DM')) -> Ptr (Ptr (StdMap StdString (Ptr DM'))) -> CInt -> CInt -> IO ()

casadi__Function__call__8
  :: Function -> M.Map String DM -> Bool -> Bool -> IO (M.Map String DM)
casadi__Function__call__8 x0 x1 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__8 errStrPtrP x0' x1' o2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__8/c_casadi__Function__call__8" else wrapReturn o2''
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o2''')



-- classy wrapper
function_call__8 :: FunctionClass a => a -> M.Map String DM -> Bool -> Bool -> IO (M.Map String DM)
function_call__8 x = casadi__Function__call__8 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__9" c_casadi__Function__call__9
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr MX')) -> Ptr (Ptr (StdVec (Ptr MX'))) -> IO ()

casadi__Function__call__9
  :: Function -> Vector MX -> IO (Vector MX)
casadi__Function__call__9 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__9 errStrPtrP x0' x1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__9/c_casadi__Function__call__9" else wrapReturn o2''

  return (o2''')



-- classy wrapper
function_call__9 :: FunctionClass a => a -> Vector MX -> IO (Vector MX)
function_call__9 x = casadi__Function__call__9 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__10" c_casadi__Function__call__10
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr MX')) -> Ptr (Ptr (StdVec (Ptr MX'))) -> CInt -> IO ()

casadi__Function__call__10
  :: Function -> Vector MX -> Bool -> IO (Vector MX)
casadi__Function__call__10 x0 x1 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__10 errStrPtrP x0' x1' o2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__10/c_casadi__Function__call__10" else wrapReturn o2''
  marshalFree x3 x3'

  return (o2''')



-- classy wrapper
function_call__10 :: FunctionClass a => a -> Vector MX -> Bool -> IO (Vector MX)
function_call__10 x = casadi__Function__call__10 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__11" c_casadi__Function__call__11
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr MX')) -> Ptr (Ptr (StdVec (Ptr MX'))) -> CInt -> CInt -> IO ()

casadi__Function__call__11
  :: Function -> Vector MX -> Bool -> Bool -> IO (Vector MX)
casadi__Function__call__11 x0 x1 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__11 errStrPtrP x0' x1' o2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__11/c_casadi__Function__call__11" else wrapReturn o2''
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o2''')



-- classy wrapper
function_call__11 :: FunctionClass a => a -> Vector MX -> Bool -> Bool -> IO (Vector MX)
function_call__11 x = casadi__Function__call__11 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__12" c_casadi__Function__call__12
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr SX')) -> Ptr (Ptr (StdVec (Ptr SX'))) -> IO ()

casadi__Function__call__12
  :: Function -> Vector SX -> IO (Vector SX)
casadi__Function__call__12 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__12 errStrPtrP x0' x1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__12/c_casadi__Function__call__12" else wrapReturn o2''

  return (o2''')



-- classy wrapper
function_call__12 :: FunctionClass a => a -> Vector SX -> IO (Vector SX)
function_call__12 x = casadi__Function__call__12 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__13" c_casadi__Function__call__13
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr SX')) -> Ptr (Ptr (StdVec (Ptr SX'))) -> CInt -> IO ()

casadi__Function__call__13
  :: Function -> Vector SX -> Bool -> IO (Vector SX)
casadi__Function__call__13 x0 x1 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__13 errStrPtrP x0' x1' o2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__13/c_casadi__Function__call__13" else wrapReturn o2''
  marshalFree x3 x3'

  return (o2''')



-- classy wrapper
function_call__13 :: FunctionClass a => a -> Vector SX -> Bool -> IO (Vector SX)
function_call__13 x = casadi__Function__call__13 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__14" c_casadi__Function__call__14
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr SX')) -> Ptr (Ptr (StdVec (Ptr SX'))) -> CInt -> CInt -> IO ()

casadi__Function__call__14
  :: Function -> Vector SX -> Bool -> Bool -> IO (Vector SX)
casadi__Function__call__14 x0 x1 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__14 errStrPtrP x0' x1' o2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__14/c_casadi__Function__call__14" else wrapReturn o2''
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o2''')



-- classy wrapper
function_call__14 :: FunctionClass a => a -> Vector SX -> Bool -> Bool -> IO (Vector SX)
function_call__14 x = casadi__Function__call__14 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__15" c_casadi__Function__call__15
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr DM')) -> Ptr (Ptr (StdVec (Ptr DM'))) -> IO ()

casadi__Function__call__15
  :: Function -> Vector DM -> IO (Vector DM)
casadi__Function__call__15 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__15 errStrPtrP x0' x1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__15/c_casadi__Function__call__15" else wrapReturn o2''

  return (o2''')



-- classy wrapper
function_call__15 :: FunctionClass a => a -> Vector DM -> IO (Vector DM)
function_call__15 x = casadi__Function__call__15 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__16" c_casadi__Function__call__16
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr DM')) -> Ptr (Ptr (StdVec (Ptr DM'))) -> CInt -> IO ()

casadi__Function__call__16
  :: Function -> Vector DM -> Bool -> IO (Vector DM)
casadi__Function__call__16 x0 x1 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__16 errStrPtrP x0' x1' o2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__16/c_casadi__Function__call__16" else wrapReturn o2''
  marshalFree x3 x3'

  return (o2''')



-- classy wrapper
function_call__16 :: FunctionClass a => a -> Vector DM -> Bool -> IO (Vector DM)
function_call__16 x = casadi__Function__call__16 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__call__17" c_casadi__Function__call__17
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr DM')) -> Ptr (Ptr (StdVec (Ptr DM'))) -> CInt -> CInt -> IO ()

casadi__Function__call__17
  :: Function -> Vector DM -> Bool -> Bool -> IO (Vector DM)
casadi__Function__call__17 x0 x1 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  o2' <- new nullPtr
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__call__17 errStrPtrP x0' x1' o2' x3' x4'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__call__17/c_casadi__Function__call__17" else wrapReturn o2''
  marshalFree x3 x3'
  marshalFree x4 x4'

  return (o2''')



-- classy wrapper
function_call__17 :: FunctionClass a => a -> Vector DM -> Bool -> Bool -> IO (Vector DM)
function_call__17 x = casadi__Function__call__17 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__checkInputs" c_casadi__Function__checkInputs
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO ()

casadi__Function__checkInputs
  :: Function -> IO ()
casadi__Function__checkInputs x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__checkInputs errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
function_checkInputs :: FunctionClass a => a -> IO ()
function_checkInputs x = casadi__Function__checkInputs (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__check_name" c_casadi__Function__check_name
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO CInt

casadi__Function__check_name
  :: String -> IO Bool
casadi__Function__check_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__check_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_check_name :: String -> IO Bool
function_check_name = casadi__Function__check_name


-- direct wrapper
foreign import ccall unsafe "casadi__Function__checkout" c_casadi__Function__checkout
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__checkout
  :: Function -> IO Int
casadi__Function__checkout x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__checkout errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_checkout :: FunctionClass a => a -> IO Int
function_checkout x = casadi__Function__checkout (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__conditional__0" c_casadi__Function__conditional__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr Function')) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__conditional__0
  :: String -> Vector Function -> Function -> IO Function
casadi__Function__conditional__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__conditional__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_conditional__0 :: String -> Vector Function -> Function -> IO Function
function_conditional__0 = casadi__Function__conditional__0


-- direct wrapper
foreign import ccall unsafe "casadi__Function__conditional__1" c_casadi__Function__conditional__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr (StdVec (Ptr Function')) -> Ptr Function' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__conditional__1
  :: String -> Vector Function -> Function -> M.Map String GenericType -> IO Function
casadi__Function__conditional__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__conditional__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_conditional__1 :: String -> Vector Function -> Function -> M.Map String GenericType -> IO Function
function_conditional__1 = casadi__Function__conditional__1


-- direct wrapper
foreign import ccall unsafe "casadi__Function__conic_debug" c_casadi__Function__conic_debug
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO ()

casadi__Function__conic_debug
  :: Function -> String -> IO ()
casadi__Function__conic_debug x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__conic_debug errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
function_conic_debug :: FunctionClass a => a -> String -> IO ()
function_conic_debug x = casadi__Function__conic_debug (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__default_in" c_casadi__Function__default_in
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CDouble

casadi__Function__default_in
  :: Function -> Int -> IO Double
casadi__Function__default_in x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__default_in errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_default_in :: FunctionClass a => a -> Int -> IO Double
function_default_in x = casadi__Function__default_in (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__expand__0" c_casadi__Function__expand__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Function')

casadi__Function__expand__0
  :: Function -> String -> IO Function
casadi__Function__expand__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__expand__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_expand__0 :: FunctionClass a => a -> String -> IO Function
function_expand__0 x = casadi__Function__expand__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__expand__1" c_casadi__Function__expand__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__expand__1
  :: Function -> String -> M.Map String GenericType -> IO Function
casadi__Function__expand__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__expand__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_expand__1 :: FunctionClass a => a -> String -> M.Map String GenericType -> IO Function
function_expand__1 x = casadi__Function__expand__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__expand__2" c_casadi__Function__expand__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__expand__2
  :: Function -> IO Function
casadi__Function__expand__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__expand__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_expand__2 :: FunctionClass a => a -> IO Function
function_expand__2 x = casadi__Function__expand__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__factory__0" c_casadi__Function__factory__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__factory__0
  :: Function -> String -> Vector String -> Vector String -> IO Function
casadi__Function__factory__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__factory__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_factory__0 :: FunctionClass a => a -> String -> Vector String -> Vector String -> IO Function
function_factory__0 x = casadi__Function__factory__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__factory__1" c_casadi__Function__factory__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr (StdVec (Ptr StdString)))) -> IO (Ptr Function')

casadi__Function__factory__1
  :: Function -> String -> Vector String -> Vector String -> M.Map String (Vector String) -> IO Function
casadi__Function__factory__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__factory__1 errStrPtrP x0' x1' x2' x3' x4'
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
function_factory__1 :: FunctionClass a => a -> String -> Vector String -> Vector String -> M.Map String (Vector String) -> IO Function
function_factory__1 x = casadi__Function__factory__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__factory__2" c_casadi__Function__factory__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr (StdVec (Ptr StdString)))) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__factory__2
  :: Function -> String -> Vector String -> Vector String -> M.Map String (Vector String) -> M.Map String GenericType -> IO Function
casadi__Function__factory__2 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__factory__2 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function_factory__2 :: FunctionClass a => a -> String -> Vector String -> Vector String -> M.Map String (Vector String) -> M.Map String GenericType -> IO Function
function_factory__2 x = casadi__Function__factory__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__fix_name" c_casadi__Function__fix_name
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

casadi__Function__fix_name
  :: String -> IO String
casadi__Function__fix_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__fix_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_fix_name :: String -> IO String
function_fix_name = casadi__Function__fix_name


-- direct wrapper
foreign import ccall unsafe "casadi__Function__forward" c_casadi__Function__forward
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

casadi__Function__forward
  :: Function -> Int -> IO Function
casadi__Function__forward x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__forward errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_forward :: FunctionClass a => a -> Int -> IO Function
function_forward x = casadi__Function__forward (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__free_mx" c_casadi__Function__free_mx
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr MX')))

casadi__Function__free_mx
  :: Function -> IO (Vector MX)
casadi__Function__free_mx x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__free_mx errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_free_mx :: FunctionClass a => a -> IO (Vector MX)
function_free_mx x = casadi__Function__free_mx (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__free_sx" c_casadi__Function__free_sx
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr SX')))

casadi__Function__free_sx
  :: Function -> IO (Vector SX)
casadi__Function__free_sx x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__free_sx errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_free_sx :: FunctionClass a => a -> IO (Vector SX)
function_free_sx x = casadi__Function__free_sx (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__fullJacobian" c_casadi__Function__fullJacobian
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__fullJacobian
  :: Function -> IO Function
casadi__Function__fullJacobian x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__fullJacobian errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_fullJacobian :: FunctionClass a => a -> IO Function
function_fullJacobian x = casadi__Function__fullJacobian (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate__0" c_casadi__Function__generate__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr StdString)

casadi__Function__generate__0
  :: Function -> IO String
casadi__Function__generate__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_generate__0 :: FunctionClass a => a -> IO String
function_generate__0 x = casadi__Function__generate__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate__1" c_casadi__Function__generate__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr StdString)

casadi__Function__generate__1
  :: Function -> M.Map String GenericType -> IO String
casadi__Function__generate__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_generate__1 :: FunctionClass a => a -> M.Map String GenericType -> IO String
function_generate__1 x = casadi__Function__generate__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate__2" c_casadi__Function__generate__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr StdString)

casadi__Function__generate__2
  :: Function -> String -> IO String
casadi__Function__generate__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_generate__2 :: FunctionClass a => a -> String -> IO String
function_generate__2 x = casadi__Function__generate__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate__3" c_casadi__Function__generate__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr StdString)

casadi__Function__generate__3
  :: Function -> String -> M.Map String GenericType -> IO String
casadi__Function__generate__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_generate__3 :: FunctionClass a => a -> String -> M.Map String GenericType -> IO String
function_generate__3 x = casadi__Function__generate__3 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate_dependencies__0" c_casadi__Function__generate_dependencies__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr StdString)

casadi__Function__generate_dependencies__0
  :: Function -> String -> IO String
casadi__Function__generate_dependencies__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate_dependencies__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_generate_dependencies__0 :: FunctionClass a => a -> String -> IO String
function_generate_dependencies__0 x = casadi__Function__generate_dependencies__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate_dependencies__1" c_casadi__Function__generate_dependencies__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr StdString)

casadi__Function__generate_dependencies__1
  :: Function -> String -> M.Map String GenericType -> IO String
casadi__Function__generate_dependencies__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate_dependencies__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_generate_dependencies__1 :: FunctionClass a => a -> String -> M.Map String GenericType -> IO String
function_generate_dependencies__1 x = casadi__Function__generate_dependencies__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__generate_lifted" c_casadi__Function__generate_lifted
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (Ptr Function') -> Ptr (Ptr Function') -> IO ()

casadi__Function__generate_lifted
  :: Function -> IO (Function, Function)
casadi__Function__generate_lifted x0 = do
  x0' <- marshal x0
  o1' <- new nullPtr
  o2' <- new nullPtr

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__generate_lifted errStrPtrP x0' o1' o2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  o1'' <- peek o1'
  free o1'
  o1''' <- if o1'' == nullPtr then error "swig output o1' was not set in casadi__Function__generate_lifted/c_casadi__Function__generate_lifted" else wrapReturn o1''
  o2'' <- peek o2'
  free o2'
  o2''' <- if o2'' == nullPtr then error "swig output o2' was not set in casadi__Function__generate_lifted/c_casadi__Function__generate_lifted" else wrapReturn o2''

  return (o1''', o2''')



-- classy wrapper
function_generate_lifted :: FunctionClass a => a -> IO (Function, Function)
function_generate_lifted x = casadi__Function__generate_lifted (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__getAlgorithmSize" c_casadi__Function__getAlgorithmSize
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__getAlgorithmSize
  :: Function -> IO Int
casadi__Function__getAlgorithmSize x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__getAlgorithmSize errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_getAlgorithmSize :: FunctionClass a => a -> IO Int
function_getAlgorithmSize x = casadi__Function__getAlgorithmSize (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__getAtomicInput" c_casadi__Function__getAtomicInput
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr (StdPair CInt CInt))

casadi__Function__getAtomicInput
  :: Function -> Int -> IO (Int, Int)
casadi__Function__getAtomicInput x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__getAtomicInput errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_getAtomicInput :: FunctionClass a => a -> Int -> IO (Int, Int)
function_getAtomicInput x = casadi__Function__getAtomicInput (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__getAtomicInputReal" c_casadi__Function__getAtomicInputReal
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CDouble

casadi__Function__getAtomicInputReal
  :: Function -> Int -> IO Double
casadi__Function__getAtomicInputReal x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__getAtomicInputReal errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_getAtomicInputReal :: FunctionClass a => a -> Int -> IO Double
function_getAtomicInputReal x = casadi__Function__getAtomicInputReal (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__getAtomicOperation" c_casadi__Function__getAtomicOperation
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__getAtomicOperation
  :: Function -> Int -> IO Int
casadi__Function__getAtomicOperation x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__getAtomicOperation errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_getAtomicOperation :: FunctionClass a => a -> Int -> IO Int
function_getAtomicOperation x = casadi__Function__getAtomicOperation (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__getAtomicOutput" c_casadi__Function__getAtomicOutput
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__getAtomicOutput
  :: Function -> Int -> IO Int
casadi__Function__getAtomicOutput x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__getAtomicOutput errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_getAtomicOutput :: FunctionClass a => a -> Int -> IO Int
function_getAtomicOutput x = casadi__Function__getAtomicOutput (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__getWorkSize" c_casadi__Function__getWorkSize
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__getWorkSize
  :: Function -> IO Int
casadi__Function__getWorkSize x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__getWorkSize errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_getWorkSize :: FunctionClass a => a -> IO Int
function_getWorkSize x = casadi__Function__getWorkSize (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__get_function__0" c_casadi__Function__get_function__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Function')

casadi__Function__get_function__0
  :: Function -> String -> IO Function
casadi__Function__get_function__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__get_function__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_get_function__0 :: FunctionClass a => a -> String -> IO Function
function_get_function__0 x = casadi__Function__get_function__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__get_function__1" c_casadi__Function__get_function__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr StdString)))

casadi__Function__get_function__1
  :: Function -> IO (Vector String)
casadi__Function__get_function__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__get_function__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_get_function__1 :: FunctionClass a => a -> IO (Vector String)
function_get_function__1 x = casadi__Function__get_function__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__has_free" c_casadi__Function__has_free
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__has_free
  :: Function -> IO Bool
casadi__Function__has_free x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__has_free errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_has_free :: FunctionClass a => a -> IO Bool
function_has_free x = casadi__Function__has_free (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__has_function" c_casadi__Function__has_function
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__has_function
  :: Function -> String -> IO Bool
casadi__Function__has_function x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__has_function errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_has_function :: FunctionClass a => a -> String -> IO Bool
function_has_function x = casadi__Function__has_function (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__0" c_casadi__Function__hessian__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> IO (Ptr Function')

casadi__Function__hessian__0
  :: Function -> String -> String -> IO Function
casadi__Function__hessian__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_hessian__0 :: FunctionClass a => a -> String -> String -> IO Function
function_hessian__0 x = casadi__Function__hessian__0 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__1" c_casadi__Function__hessian__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> IO (Ptr Function')

casadi__Function__hessian__1
  :: Function -> Int -> String -> IO Function
casadi__Function__hessian__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_hessian__1 :: FunctionClass a => a -> Int -> String -> IO Function
function_hessian__1 x = casadi__Function__hessian__1 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__2" c_casadi__Function__hessian__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Function')

casadi__Function__hessian__2
  :: Function -> String -> IO Function
casadi__Function__hessian__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_hessian__2 :: FunctionClass a => a -> String -> IO Function
function_hessian__2 x = casadi__Function__hessian__2 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__3" c_casadi__Function__hessian__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> IO (Ptr Function')

casadi__Function__hessian__3
  :: Function -> String -> Int -> IO Function
casadi__Function__hessian__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_hessian__3 :: FunctionClass a => a -> String -> Int -> IO Function
function_hessian__3 x = casadi__Function__hessian__3 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__4" c_casadi__Function__hessian__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__hessian__4
  :: Function -> IO Function
casadi__Function__hessian__4 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__4 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_hessian__4 :: FunctionClass a => a -> IO Function
function_hessian__4 x = casadi__Function__hessian__4 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__5" c_casadi__Function__hessian__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

casadi__Function__hessian__5
  :: Function -> Int -> IO Function
casadi__Function__hessian__5 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__5 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_hessian__5 :: FunctionClass a => a -> Int -> IO Function
function_hessian__5 x = casadi__Function__hessian__5 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__hessian__6" c_casadi__Function__hessian__6
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__hessian__6
  :: Function -> Int -> Int -> IO Function
casadi__Function__hessian__6 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__hessian__6 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_hessian__6 :: FunctionClass a => a -> Int -> Int -> IO Function
function_hessian__6 x = casadi__Function__hessian__6 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__if_else__0" c_casadi__Function__if_else__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Function' -> Ptr Function' -> IO (Ptr Function')

casadi__Function__if_else__0
  :: String -> Function -> Function -> IO Function
casadi__Function__if_else__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__if_else__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_if_else__0 :: String -> Function -> Function -> IO Function
function_if_else__0 = casadi__Function__if_else__0


-- direct wrapper
foreign import ccall unsafe "casadi__Function__if_else__1" c_casadi__Function__if_else__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Function' -> Ptr Function' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__if_else__1
  :: String -> Function -> Function -> M.Map String GenericType -> IO Function
casadi__Function__if_else__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__if_else__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_if_else__1 :: String -> Function -> Function -> M.Map String GenericType -> IO Function
function_if_else__1 = casadi__Function__if_else__1


-- direct wrapper
foreign import ccall unsafe "casadi__Function__index_in" c_casadi__Function__index_in
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__index_in
  :: Function -> String -> IO Int
casadi__Function__index_in x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__index_in errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_index_in :: FunctionClass a => a -> String -> IO Int
function_index_in x = casadi__Function__index_in (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__index_out" c_casadi__Function__index_out
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__index_out
  :: Function -> String -> IO Int
casadi__Function__index_out x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__index_out errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_index_out :: FunctionClass a => a -> String -> IO Int
function_index_out x = casadi__Function__index_out (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__is_a__0" c_casadi__Function__is_a__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__is_a__0
  :: Function -> String -> IO Bool
casadi__Function__is_a__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__is_a__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_is_a__0 :: FunctionClass a => a -> String -> IO Bool
function_is_a__0 x = casadi__Function__is_a__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__is_a__1" c_casadi__Function__is_a__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> IO CInt

casadi__Function__is_a__1
  :: Function -> String -> Bool -> IO Bool
casadi__Function__is_a__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__is_a__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_is_a__1 :: FunctionClass a => a -> String -> Bool -> IO Bool
function_is_a__1 x = casadi__Function__is_a__1 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__0" c_casadi__Function__jacobian__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> IO (Ptr Function')

casadi__Function__jacobian__0
  :: Function -> String -> String -> IO Function
casadi__Function__jacobian__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_jacobian__0 :: FunctionClass a => a -> String -> String -> IO Function
function_jacobian__0 x = casadi__Function__jacobian__0 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__1" c_casadi__Function__jacobian__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__1
  :: Function -> String -> String -> Bool -> IO Function
casadi__Function__jacobian__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_jacobian__1 :: FunctionClass a => a -> String -> String -> Bool -> IO Function
function_jacobian__1 x = casadi__Function__jacobian__1 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__2" c_casadi__Function__jacobian__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__2
  :: Function -> String -> String -> Bool -> Bool -> IO Function
casadi__Function__jacobian__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__2 errStrPtrP x0' x1' x2' x3' x4'
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
function_jacobian__2 :: FunctionClass a => a -> String -> String -> Bool -> Bool -> IO Function
function_jacobian__2 x = casadi__Function__jacobian__2 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__3" c_casadi__Function__jacobian__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> IO (Ptr Function')

casadi__Function__jacobian__3
  :: Function -> Int -> String -> IO Function
casadi__Function__jacobian__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_jacobian__3 :: FunctionClass a => a -> Int -> String -> IO Function
function_jacobian__3 x = casadi__Function__jacobian__3 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__4" c_casadi__Function__jacobian__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__4
  :: Function -> Int -> String -> Bool -> IO Function
casadi__Function__jacobian__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_jacobian__4 :: FunctionClass a => a -> Int -> String -> Bool -> IO Function
function_jacobian__4 x = casadi__Function__jacobian__4 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__5" c_casadi__Function__jacobian__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__5
  :: Function -> Int -> String -> Bool -> Bool -> IO Function
casadi__Function__jacobian__5 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__5 errStrPtrP x0' x1' x2' x3' x4'
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
function_jacobian__5 :: FunctionClass a => a -> Int -> String -> Bool -> Bool -> IO Function
function_jacobian__5 x = casadi__Function__jacobian__5 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__6" c_casadi__Function__jacobian__6
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Function')

casadi__Function__jacobian__6
  :: Function -> String -> IO Function
casadi__Function__jacobian__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_jacobian__6 :: FunctionClass a => a -> String -> IO Function
function_jacobian__6 x = casadi__Function__jacobian__6 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__7" c_casadi__Function__jacobian__7
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__7
  :: Function -> String -> Int -> IO Function
casadi__Function__jacobian__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_jacobian__7 :: FunctionClass a => a -> String -> Int -> IO Function
function_jacobian__7 x = casadi__Function__jacobian__7 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__8" c_casadi__Function__jacobian__8
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__8
  :: Function -> String -> Int -> Bool -> IO Function
casadi__Function__jacobian__8 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__8 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_jacobian__8 :: FunctionClass a => a -> String -> Int -> Bool -> IO Function
function_jacobian__8 x = casadi__Function__jacobian__8 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__9" c_casadi__Function__jacobian__9
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__9
  :: Function -> String -> Int -> Bool -> Bool -> IO Function
casadi__Function__jacobian__9 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__9 errStrPtrP x0' x1' x2' x3' x4'
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
function_jacobian__9 :: FunctionClass a => a -> String -> Int -> Bool -> Bool -> IO Function
function_jacobian__9 x = casadi__Function__jacobian__9 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__10" c_casadi__Function__jacobian__10
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__jacobian__10
  :: Function -> IO Function
casadi__Function__jacobian__10 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__10 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_jacobian__10 :: FunctionClass a => a -> IO Function
function_jacobian__10 x = casadi__Function__jacobian__10 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__11" c_casadi__Function__jacobian__11
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__11
  :: Function -> Int -> IO Function
casadi__Function__jacobian__11 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__11 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_jacobian__11 :: FunctionClass a => a -> Int -> IO Function
function_jacobian__11 x = casadi__Function__jacobian__11 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__12" c_casadi__Function__jacobian__12
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__12
  :: Function -> Int -> Int -> IO Function
casadi__Function__jacobian__12 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__12 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_jacobian__12 :: FunctionClass a => a -> Int -> Int -> IO Function
function_jacobian__12 x = casadi__Function__jacobian__12 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__13" c_casadi__Function__jacobian__13
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__13
  :: Function -> Int -> Int -> Bool -> IO Function
casadi__Function__jacobian__13 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__13 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_jacobian__13 :: FunctionClass a => a -> Int -> Int -> Bool -> IO Function
function_jacobian__13 x = casadi__Function__jacobian__13 (castFunction x)


-- direct wrapper
foreign import ccall safe "casadi__Function__jacobian__14" c_casadi__Function__jacobian__14
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__jacobian__14
  :: Function -> Int -> Int -> Bool -> Bool -> IO Function
casadi__Function__jacobian__14 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__jacobian__14 errStrPtrP x0' x1' x2' x3' x4'
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
function_jacobian__14 :: FunctionClass a => a -> Int -> Int -> Bool -> Bool -> IO Function
function_jacobian__14 x = casadi__Function__jacobian__14 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__map__0" c_casadi__Function__map__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__map__0
  :: Function -> String -> String -> Int -> Vector String -> Vector String -> IO Function
casadi__Function__map__0 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__map__0 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function_map__0 :: FunctionClass a => a -> String -> String -> Int -> Vector String -> Vector String -> IO Function
function_map__0 x = casadi__Function__map__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__map__1" c_casadi__Function__map__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__map__1
  :: Function -> String -> String -> Int -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Function__map__1 x0 x1 x2 x3 x4 x5 x6 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5
  x6' <- marshal x6

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__map__1 errStrPtrP x0' x1' x2' x3' x4' x5' x6'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'
  marshalFree x6 x6'

  return ret



-- classy wrapper
function_map__1 :: FunctionClass a => a -> String -> String -> Int -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
function_map__1 x = casadi__Function__map__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__map__2" c_casadi__Function__map__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr Function')

casadi__Function__map__2
  :: Function -> String -> String -> Int -> Vector Int -> Vector Int -> IO Function
casadi__Function__map__2 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__map__2 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function_map__2 :: FunctionClass a => a -> String -> String -> Int -> Vector Int -> Vector Int -> IO Function
function_map__2 x = casadi__Function__map__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__map__3" c_casadi__Function__map__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__map__3
  :: Function -> String -> String -> Int -> Vector Int -> Vector Int -> M.Map String GenericType -> IO Function
casadi__Function__map__3 x0 x1 x2 x3 x4 x5 x6 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5
  x6' <- marshal x6

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__map__3 errStrPtrP x0' x1' x2' x3' x4' x5' x6'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'
  marshalFree x6 x6'

  return ret



-- classy wrapper
function_map__3 :: FunctionClass a => a -> String -> String -> Int -> Vector Int -> Vector Int -> M.Map String GenericType -> IO Function
function_map__3 x = casadi__Function__map__3 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__map__4" c_casadi__Function__map__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

casadi__Function__map__4
  :: Function -> Int -> IO Function
casadi__Function__map__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__map__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_map__4 :: FunctionClass a => a -> Int -> IO Function
function_map__4 x = casadi__Function__map__4 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__map__5" c_casadi__Function__map__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> IO (Ptr Function')

casadi__Function__map__5
  :: Function -> Int -> String -> IO Function
casadi__Function__map__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__map__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_map__5 :: FunctionClass a => a -> Int -> String -> IO Function
function_map__5 x = casadi__Function__map__5 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__0" c_casadi__Function__mapaccum__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> IO (Ptr Function')

casadi__Function__mapaccum__0
  :: Function -> String -> Int -> Vector String -> Vector String -> IO Function
casadi__Function__mapaccum__0 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__0 errStrPtrP x0' x1' x2' x3' x4'
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
function_mapaccum__0 :: FunctionClass a => a -> String -> Int -> Vector String -> Vector String -> IO Function
function_mapaccum__0 x = casadi__Function__mapaccum__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__1" c_casadi__Function__mapaccum__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__mapaccum__1
  :: Function -> String -> Int -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
casadi__Function__mapaccum__1 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__1 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function_mapaccum__1 :: FunctionClass a => a -> String -> Int -> Vector String -> Vector String -> M.Map String GenericType -> IO Function
function_mapaccum__1 x = casadi__Function__mapaccum__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__2" c_casadi__Function__mapaccum__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr Function')

casadi__Function__mapaccum__2
  :: Function -> String -> Int -> Vector Int -> Vector Int -> IO Function
casadi__Function__mapaccum__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__2 errStrPtrP x0' x1' x2' x3' x4'
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
function_mapaccum__2 :: FunctionClass a => a -> String -> Int -> Vector Int -> Vector Int -> IO Function
function_mapaccum__2 x = casadi__Function__mapaccum__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__3" c_casadi__Function__mapaccum__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__mapaccum__3
  :: Function -> String -> Int -> Vector Int -> Vector Int -> M.Map String GenericType -> IO Function
casadi__Function__mapaccum__3 x0 x1 x2 x3 x4 x5 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4
  x5' <- marshal x5

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__3 errStrPtrP x0' x1' x2' x3' x4' x5'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'
  marshalFree x4 x4'
  marshalFree x5 x5'

  return ret



-- classy wrapper
function_mapaccum__3 :: FunctionClass a => a -> String -> Int -> Vector Int -> Vector Int -> M.Map String GenericType -> IO Function
function_mapaccum__3 x = casadi__Function__mapaccum__3 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__4" c_casadi__Function__mapaccum__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> IO (Ptr Function')

casadi__Function__mapaccum__4
  :: Function -> String -> Int -> IO Function
casadi__Function__mapaccum__4 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__4 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_mapaccum__4 :: FunctionClass a => a -> String -> Int -> IO Function
function_mapaccum__4 x = casadi__Function__mapaccum__4 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__5" c_casadi__Function__mapaccum__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> CInt -> IO (Ptr Function')

casadi__Function__mapaccum__5
  :: Function -> String -> Int -> Int -> IO Function
casadi__Function__mapaccum__5 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__5 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_mapaccum__5 :: FunctionClass a => a -> String -> Int -> Int -> IO Function
function_mapaccum__5 x = casadi__Function__mapaccum__5 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapaccum__6" c_casadi__Function__mapaccum__6
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> CInt -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__mapaccum__6
  :: Function -> String -> Int -> Int -> M.Map String GenericType -> IO Function
casadi__Function__mapaccum__6 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapaccum__6 errStrPtrP x0' x1' x2' x3' x4'
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
function_mapaccum__6 :: FunctionClass a => a -> String -> Int -> Int -> M.Map String GenericType -> IO Function
function_mapaccum__6 x = casadi__Function__mapaccum__6 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapsum__0" c_casadi__Function__mapsum__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr (StdVec (Ptr MX')))

casadi__Function__mapsum__0
  :: Function -> Vector MX -> IO (Vector MX)
casadi__Function__mapsum__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapsum__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_mapsum__0 :: FunctionClass a => a -> Vector MX -> IO (Vector MX)
function_mapsum__0 x = casadi__Function__mapsum__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mapsum__1" c_casadi__Function__mapsum__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr (StdVec (Ptr MX')) -> Ptr StdString -> IO (Ptr (StdVec (Ptr MX')))

casadi__Function__mapsum__1
  :: Function -> Vector MX -> String -> IO (Vector MX)
casadi__Function__mapsum__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mapsum__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_mapsum__1 :: FunctionClass a => a -> Vector MX -> String -> IO (Vector MX)
function_mapsum__1 x = casadi__Function__mapsum__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mx_in__0" c_casadi__Function__mx_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr MX')))

casadi__Function__mx_in__0
  :: Function -> IO (Vector MX)
casadi__Function__mx_in__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mx_in__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_mx_in__0 :: FunctionClass a => a -> IO (Vector MX)
function_mx_in__0 x = casadi__Function__mx_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mx_in__1" c_casadi__Function__mx_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr MX')

casadi__Function__mx_in__1
  :: Function -> String -> IO MX
casadi__Function__mx_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mx_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_mx_in__1 :: FunctionClass a => a -> String -> IO MX
function_mx_in__1 x = casadi__Function__mx_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mx_in__2" c_casadi__Function__mx_in__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr MX')

casadi__Function__mx_in__2
  :: Function -> Int -> IO MX
casadi__Function__mx_in__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mx_in__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_mx_in__2 :: FunctionClass a => a -> Int -> IO MX
function_mx_in__2 x = casadi__Function__mx_in__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mx_out__0" c_casadi__Function__mx_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr MX')))

casadi__Function__mx_out__0
  :: Function -> IO (Vector MX)
casadi__Function__mx_out__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mx_out__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_mx_out__0 :: FunctionClass a => a -> IO (Vector MX)
function_mx_out__0 x = casadi__Function__mx_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mx_out__1" c_casadi__Function__mx_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr MX')

casadi__Function__mx_out__1
  :: Function -> String -> IO MX
casadi__Function__mx_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mx_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_mx_out__1 :: FunctionClass a => a -> String -> IO MX
function_mx_out__1 x = casadi__Function__mx_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__mx_out__2" c_casadi__Function__mx_out__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr MX')

casadi__Function__mx_out__2
  :: Function -> Int -> IO MX
casadi__Function__mx_out__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__mx_out__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_mx_out__2 :: FunctionClass a => a -> Int -> IO MX
function_mx_out__2 x = casadi__Function__mx_out__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__n_in" c_casadi__Function__n_in
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__n_in
  :: Function -> IO Int
casadi__Function__n_in x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__n_in errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_n_in :: FunctionClass a => a -> IO Int
function_n_in x = casadi__Function__n_in (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__n_nodes" c_casadi__Function__n_nodes
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__n_nodes
  :: Function -> IO Int
casadi__Function__n_nodes x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__n_nodes errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_n_nodes :: FunctionClass a => a -> IO Int
function_n_nodes x = casadi__Function__n_nodes (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__n_out" c_casadi__Function__n_out
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__n_out
  :: Function -> IO Int
casadi__Function__n_out x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__n_out errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_n_out :: FunctionClass a => a -> IO Int
function_n_out x = casadi__Function__n_out (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__name" c_casadi__Function__name
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr StdString)

casadi__Function__name
  :: Function -> IO String
casadi__Function__name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_name :: FunctionClass a => a -> IO String
function_name x = casadi__Function__name (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__name_in__0" c_casadi__Function__name_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr StdString)

casadi__Function__name_in__0
  :: Function -> Int -> IO String
casadi__Function__name_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__name_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_name_in__0 :: FunctionClass a => a -> Int -> IO String
function_name_in__0 x = casadi__Function__name_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__name_in__1" c_casadi__Function__name_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr StdString)))

casadi__Function__name_in__1
  :: Function -> IO (Vector String)
casadi__Function__name_in__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__name_in__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_name_in__1 :: FunctionClass a => a -> IO (Vector String)
function_name_in__1 x = casadi__Function__name_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__name_out__0" c_casadi__Function__name_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr StdString)

casadi__Function__name_out__0
  :: Function -> Int -> IO String
casadi__Function__name_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__name_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_name_out__0 :: FunctionClass a => a -> Int -> IO String
function_name_out__0 x = casadi__Function__name_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__name_out__1" c_casadi__Function__name_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr StdString)))

casadi__Function__name_out__1
  :: Function -> IO (Vector String)
casadi__Function__name_out__1 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__name_out__1 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_name_out__1 :: FunctionClass a => a -> IO (Vector String)
function_name_out__1 x = casadi__Function__name_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__nnz_in__0" c_casadi__Function__nnz_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__nnz_in__0
  :: Function -> String -> IO Int
casadi__Function__nnz_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__nnz_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_nnz_in__0 :: FunctionClass a => a -> String -> IO Int
function_nnz_in__0 x = casadi__Function__nnz_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__nnz_in__1" c_casadi__Function__nnz_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__nnz_in__1
  :: Function -> Int -> IO Int
casadi__Function__nnz_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__nnz_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_nnz_in__1 :: FunctionClass a => a -> Int -> IO Int
function_nnz_in__1 x = casadi__Function__nnz_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__nnz_in__2" c_casadi__Function__nnz_in__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__nnz_in__2
  :: Function -> IO Int
casadi__Function__nnz_in__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__nnz_in__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_nnz_in__2 :: FunctionClass a => a -> IO Int
function_nnz_in__2 x = casadi__Function__nnz_in__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__nnz_out__0" c_casadi__Function__nnz_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__nnz_out__0
  :: Function -> String -> IO Int
casadi__Function__nnz_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__nnz_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_nnz_out__0 :: FunctionClass a => a -> String -> IO Int
function_nnz_out__0 x = casadi__Function__nnz_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__nnz_out__1" c_casadi__Function__nnz_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__nnz_out__1
  :: Function -> Int -> IO Int
casadi__Function__nnz_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__nnz_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_nnz_out__1 :: FunctionClass a => a -> Int -> IO Int
function_nnz_out__1 x = casadi__Function__nnz_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__nnz_out__2" c_casadi__Function__nnz_out__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__nnz_out__2
  :: Function -> IO Int
casadi__Function__nnz_out__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__nnz_out__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_nnz_out__2 :: FunctionClass a => a -> IO Int
function_nnz_out__2 x = casadi__Function__nnz_out__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__numel_in__0" c_casadi__Function__numel_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__numel_in__0
  :: Function -> String -> IO Int
casadi__Function__numel_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__numel_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_numel_in__0 :: FunctionClass a => a -> String -> IO Int
function_numel_in__0 x = casadi__Function__numel_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__numel_in__1" c_casadi__Function__numel_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__numel_in__1
  :: Function -> Int -> IO Int
casadi__Function__numel_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__numel_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_numel_in__1 :: FunctionClass a => a -> Int -> IO Int
function_numel_in__1 x = casadi__Function__numel_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__numel_in__2" c_casadi__Function__numel_in__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__numel_in__2
  :: Function -> IO Int
casadi__Function__numel_in__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__numel_in__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_numel_in__2 :: FunctionClass a => a -> IO Int
function_numel_in__2 x = casadi__Function__numel_in__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__numel_out__0" c_casadi__Function__numel_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__numel_out__0
  :: Function -> String -> IO Int
casadi__Function__numel_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__numel_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_numel_out__0 :: FunctionClass a => a -> String -> IO Int
function_numel_out__0 x = casadi__Function__numel_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__numel_out__1" c_casadi__Function__numel_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__numel_out__1
  :: Function -> Int -> IO Int
casadi__Function__numel_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__numel_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_numel_out__1 :: FunctionClass a => a -> Int -> IO Int
function_numel_out__1 x = casadi__Function__numel_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__numel_out__2" c_casadi__Function__numel_out__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CInt

casadi__Function__numel_out__2
  :: Function -> IO Int
casadi__Function__numel_out__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__numel_out__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_numel_out__2 :: FunctionClass a => a -> IO Int
function_numel_out__2 x = casadi__Function__numel_out__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__oracle" c_casadi__Function__oracle
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__oracle
  :: Function -> IO Function
casadi__Function__oracle x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__oracle errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_oracle :: FunctionClass a => a -> IO Function
function_oracle x = casadi__Function__oracle (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__print_dimensions" c_casadi__Function__print_dimensions
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO ()

casadi__Function__print_dimensions
  :: Function -> IO ()
casadi__Function__print_dimensions x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__print_dimensions errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
function_print_dimensions :: FunctionClass a => a -> IO ()
function_print_dimensions x = casadi__Function__print_dimensions (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__print_free" c_casadi__Function__print_free
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO ()

casadi__Function__print_free
  :: Function -> IO ()
casadi__Function__print_free x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__print_free errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
function_print_free :: FunctionClass a => a -> IO ()
function_print_free x = casadi__Function__print_free (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__print_option" c_casadi__Function__print_option
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO ()

casadi__Function__print_option
  :: Function -> String -> IO ()
casadi__Function__print_option x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__print_option errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
function_print_option :: FunctionClass a => a -> String -> IO ()
function_print_option x = casadi__Function__print_option (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__print_options" c_casadi__Function__print_options
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO ()

casadi__Function__print_options
  :: Function -> IO ()
casadi__Function__print_options x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__print_options errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
function_print_options :: FunctionClass a => a -> IO ()
function_print_options x = casadi__Function__print_options (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__release" c_casadi__Function__release
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO ()

casadi__Function__release
  :: Function -> Int -> IO ()
casadi__Function__release x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__release errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
function_release :: FunctionClass a => a -> Int -> IO ()
function_release x = casadi__Function__release (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__reverse" c_casadi__Function__reverse
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Function')

casadi__Function__reverse
  :: Function -> Int -> IO Function
casadi__Function__reverse x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__reverse errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_reverse :: FunctionClass a => a -> Int -> IO Function
function_reverse x = casadi__Function__reverse (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__setFullJacobian" c_casadi__Function__setFullJacobian
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr Function' -> IO ()

casadi__Function__setFullJacobian
  :: Function -> Function -> IO ()
casadi__Function__setFullJacobian x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__setFullJacobian errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
function_setFullJacobian :: FunctionClass a => a -> Function -> IO ()
function_setFullJacobian x = casadi__Function__setFullJacobian (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size1_in__0" c_casadi__Function__size1_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__size1_in__0
  :: Function -> String -> IO Int
casadi__Function__size1_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size1_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size1_in__0 :: FunctionClass a => a -> String -> IO Int
function_size1_in__0 x = casadi__Function__size1_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size1_in__1" c_casadi__Function__size1_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__size1_in__1
  :: Function -> Int -> IO Int
casadi__Function__size1_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size1_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size1_in__1 :: FunctionClass a => a -> Int -> IO Int
function_size1_in__1 x = casadi__Function__size1_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size1_out__0" c_casadi__Function__size1_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__size1_out__0
  :: Function -> String -> IO Int
casadi__Function__size1_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size1_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size1_out__0 :: FunctionClass a => a -> String -> IO Int
function_size1_out__0 x = casadi__Function__size1_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size1_out__1" c_casadi__Function__size1_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__size1_out__1
  :: Function -> Int -> IO Int
casadi__Function__size1_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size1_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size1_out__1 :: FunctionClass a => a -> Int -> IO Int
function_size1_out__1 x = casadi__Function__size1_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size2_in__0" c_casadi__Function__size2_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__size2_in__0
  :: Function -> String -> IO Int
casadi__Function__size2_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size2_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size2_in__0 :: FunctionClass a => a -> String -> IO Int
function_size2_in__0 x = casadi__Function__size2_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size2_in__1" c_casadi__Function__size2_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__size2_in__1
  :: Function -> Int -> IO Int
casadi__Function__size2_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size2_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size2_in__1 :: FunctionClass a => a -> Int -> IO Int
function_size2_in__1 x = casadi__Function__size2_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size2_out__0" c_casadi__Function__size2_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO CInt

casadi__Function__size2_out__0
  :: Function -> String -> IO Int
casadi__Function__size2_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size2_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size2_out__0 :: FunctionClass a => a -> String -> IO Int
function_size2_out__0 x = casadi__Function__size2_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size2_out__1" c_casadi__Function__size2_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__size2_out__1
  :: Function -> Int -> IO Int
casadi__Function__size2_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size2_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size2_out__1 :: FunctionClass a => a -> Int -> IO Int
function_size2_out__1 x = casadi__Function__size2_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size_in__0" c_casadi__Function__size_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr (StdPair CInt CInt))

casadi__Function__size_in__0
  :: Function -> String -> IO (Int, Int)
casadi__Function__size_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size_in__0 :: FunctionClass a => a -> String -> IO (Int, Int)
function_size_in__0 x = casadi__Function__size_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size_in__1" c_casadi__Function__size_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr (StdPair CInt CInt))

casadi__Function__size_in__1
  :: Function -> Int -> IO (Int, Int)
casadi__Function__size_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size_in__1 :: FunctionClass a => a -> Int -> IO (Int, Int)
function_size_in__1 x = casadi__Function__size_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size_out__0" c_casadi__Function__size_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr (StdPair CInt CInt))

casadi__Function__size_out__0
  :: Function -> String -> IO (Int, Int)
casadi__Function__size_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size_out__0 :: FunctionClass a => a -> String -> IO (Int, Int)
function_size_out__0 x = casadi__Function__size_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__size_out__1" c_casadi__Function__size_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr (StdPair CInt CInt))

casadi__Function__size_out__1
  :: Function -> Int -> IO (Int, Int)
casadi__Function__size_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__size_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_size_out__1 :: FunctionClass a => a -> Int -> IO (Int, Int)
function_size_out__1 x = casadi__Function__size_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__slice__0" c_casadi__Function__slice__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> IO (Ptr Function')

casadi__Function__slice__0
  :: Function -> String -> Vector Int -> Vector Int -> IO Function
casadi__Function__slice__0 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__slice__0 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_slice__0 :: FunctionClass a => a -> String -> Vector Int -> Vector Int -> IO Function
function_slice__0 x = casadi__Function__slice__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__slice__1" c_casadi__Function__slice__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdVec CInt) -> Ptr (StdVec CInt) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO (Ptr Function')

casadi__Function__slice__1
  :: Function -> String -> Vector Int -> Vector Int -> M.Map String GenericType -> IO Function
casadi__Function__slice__1 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__slice__1 errStrPtrP x0' x1' x2' x3' x4'
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
function_slice__1 :: FunctionClass a => a -> String -> Vector Int -> Vector Int -> M.Map String GenericType -> IO Function
function_slice__1 x = casadi__Function__slice__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__spCanEvaluate" c_casadi__Function__spCanEvaluate
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO CInt

casadi__Function__spCanEvaluate
  :: Function -> Bool -> IO Bool
casadi__Function__spCanEvaluate x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__spCanEvaluate errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_spCanEvaluate :: FunctionClass a => a -> Bool -> IO Bool
function_spCanEvaluate x = casadi__Function__spCanEvaluate (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_in__0" c_casadi__Function__sparsity_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Sparsity')

casadi__Function__sparsity_in__0
  :: Function -> String -> IO Sparsity
casadi__Function__sparsity_in__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_in__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sparsity_in__0 :: FunctionClass a => a -> String -> IO Sparsity
function_sparsity_in__0 x = casadi__Function__sparsity_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_in__1" c_casadi__Function__sparsity_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_in__1
  :: Function -> Int -> IO Sparsity
casadi__Function__sparsity_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sparsity_in__1 :: FunctionClass a => a -> Int -> IO Sparsity
function_sparsity_in__1 x = casadi__Function__sparsity_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__0" c_casadi__Function__sparsity_jac__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__0
  :: Function -> String -> String -> IO Sparsity
casadi__Function__sparsity_jac__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_sparsity_jac__0 :: FunctionClass a => a -> String -> String -> IO Sparsity
function_sparsity_jac__0 x = casadi__Function__sparsity_jac__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__1" c_casadi__Function__sparsity_jac__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__1
  :: Function -> String -> String -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__1 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__1 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_sparsity_jac__1 :: FunctionClass a => a -> String -> String -> Bool -> IO Sparsity
function_sparsity_jac__1 x = casadi__Function__sparsity_jac__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__2" c_casadi__Function__sparsity_jac__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr StdString -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__2
  :: Function -> String -> String -> Bool -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__2 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__2 errStrPtrP x0' x1' x2' x3' x4'
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
function_sparsity_jac__2 :: FunctionClass a => a -> String -> String -> Bool -> Bool -> IO Sparsity
function_sparsity_jac__2 x = casadi__Function__sparsity_jac__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__3" c_casadi__Function__sparsity_jac__3
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__3
  :: Function -> Int -> String -> IO Sparsity
casadi__Function__sparsity_jac__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_sparsity_jac__3 :: FunctionClass a => a -> Int -> String -> IO Sparsity
function_sparsity_jac__3 x = casadi__Function__sparsity_jac__3 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__4" c_casadi__Function__sparsity_jac__4
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__4
  :: Function -> Int -> String -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__4 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__4 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_sparsity_jac__4 :: FunctionClass a => a -> Int -> String -> Bool -> IO Sparsity
function_sparsity_jac__4 x = casadi__Function__sparsity_jac__4 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__5" c_casadi__Function__sparsity_jac__5
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> Ptr StdString -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__5
  :: Function -> Int -> String -> Bool -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__5 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__5 errStrPtrP x0' x1' x2' x3' x4'
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
function_sparsity_jac__5 :: FunctionClass a => a -> Int -> String -> Bool -> Bool -> IO Sparsity
function_sparsity_jac__5 x = casadi__Function__sparsity_jac__5 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__6" c_casadi__Function__sparsity_jac__6
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__6
  :: Function -> String -> IO Sparsity
casadi__Function__sparsity_jac__6 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__6 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sparsity_jac__6 :: FunctionClass a => a -> String -> IO Sparsity
function_sparsity_jac__6 x = casadi__Function__sparsity_jac__6 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__7" c_casadi__Function__sparsity_jac__7
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__7
  :: Function -> String -> Int -> IO Sparsity
casadi__Function__sparsity_jac__7 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__7 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_sparsity_jac__7 :: FunctionClass a => a -> String -> Int -> IO Sparsity
function_sparsity_jac__7 x = casadi__Function__sparsity_jac__7 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__8" c_casadi__Function__sparsity_jac__8
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__8
  :: Function -> String -> Int -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__8 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__8 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_sparsity_jac__8 :: FunctionClass a => a -> String -> Int -> Bool -> IO Sparsity
function_sparsity_jac__8 x = casadi__Function__sparsity_jac__8 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__9" c_casadi__Function__sparsity_jac__9
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> CInt -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__9
  :: Function -> String -> Int -> Bool -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__9 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__9 errStrPtrP x0' x1' x2' x3' x4'
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
function_sparsity_jac__9 :: FunctionClass a => a -> String -> Int -> Bool -> Bool -> IO Sparsity
function_sparsity_jac__9 x = casadi__Function__sparsity_jac__9 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__10" c_casadi__Function__sparsity_jac__10
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__10
  :: Function -> IO Sparsity
casadi__Function__sparsity_jac__10 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__10 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sparsity_jac__10 :: FunctionClass a => a -> IO Sparsity
function_sparsity_jac__10 x = casadi__Function__sparsity_jac__10 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__11" c_casadi__Function__sparsity_jac__11
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__11
  :: Function -> Int -> IO Sparsity
casadi__Function__sparsity_jac__11 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__11 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sparsity_jac__11 :: FunctionClass a => a -> Int -> IO Sparsity
function_sparsity_jac__11 x = casadi__Function__sparsity_jac__11 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__12" c_casadi__Function__sparsity_jac__12
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__12
  :: Function -> Int -> Int -> IO Sparsity
casadi__Function__sparsity_jac__12 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__12 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
function_sparsity_jac__12 :: FunctionClass a => a -> Int -> Int -> IO Sparsity
function_sparsity_jac__12 x = casadi__Function__sparsity_jac__12 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__13" c_casadi__Function__sparsity_jac__13
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__13
  :: Function -> Int -> Int -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__13 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__13 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
function_sparsity_jac__13 :: FunctionClass a => a -> Int -> Int -> Bool -> IO Sparsity
function_sparsity_jac__13 x = casadi__Function__sparsity_jac__13 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_jac__14" c_casadi__Function__sparsity_jac__14
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_jac__14
  :: Function -> Int -> Int -> Bool -> Bool -> IO Sparsity
casadi__Function__sparsity_jac__14 x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_jac__14 errStrPtrP x0' x1' x2' x3' x4'
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
function_sparsity_jac__14 :: FunctionClass a => a -> Int -> Int -> Bool -> Bool -> IO Sparsity
function_sparsity_jac__14 x = casadi__Function__sparsity_jac__14 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_out__0" c_casadi__Function__sparsity_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr Sparsity')

casadi__Function__sparsity_out__0
  :: Function -> String -> IO Sparsity
casadi__Function__sparsity_out__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_out__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sparsity_out__0 :: FunctionClass a => a -> String -> IO Sparsity
function_sparsity_out__0 x = casadi__Function__sparsity_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sparsity_out__1" c_casadi__Function__sparsity_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr Sparsity')

casadi__Function__sparsity_out__1
  :: Function -> Int -> IO Sparsity
casadi__Function__sparsity_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sparsity_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sparsity_out__1 :: FunctionClass a => a -> Int -> IO Sparsity
function_sparsity_out__1 x = casadi__Function__sparsity_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__stats__0" c_casadi__Function__stats__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__Function__stats__0
  :: Function -> IO (M.Map String GenericType)
casadi__Function__stats__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__stats__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_stats__0 :: FunctionClass a => a -> IO (M.Map String GenericType)
function_stats__0 x = casadi__Function__stats__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__stats__1" c_casadi__Function__stats__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__Function__stats__1
  :: Function -> Int -> IO (M.Map String GenericType)
casadi__Function__stats__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__stats__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_stats__1 :: FunctionClass a => a -> Int -> IO (M.Map String GenericType)
function_stats__1 x = casadi__Function__stats__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sx_in__0" c_casadi__Function__sx_in__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr SX')))

casadi__Function__sx_in__0
  :: Function -> IO (Vector SX)
casadi__Function__sx_in__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sx_in__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sx_in__0 :: FunctionClass a => a -> IO (Vector SX)
function_sx_in__0 x = casadi__Function__sx_in__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sx_in__1" c_casadi__Function__sx_in__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr SX')

casadi__Function__sx_in__1
  :: Function -> String -> IO SX
casadi__Function__sx_in__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sx_in__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sx_in__1 :: FunctionClass a => a -> String -> IO SX
function_sx_in__1 x = casadi__Function__sx_in__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sx_in__2" c_casadi__Function__sx_in__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr SX')

casadi__Function__sx_in__2
  :: Function -> Int -> IO SX
casadi__Function__sx_in__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sx_in__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sx_in__2 :: FunctionClass a => a -> Int -> IO SX
function_sx_in__2 x = casadi__Function__sx_in__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sx_out__0" c_casadi__Function__sx_out__0
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr (StdVec (Ptr SX')))

casadi__Function__sx_out__0
  :: Function -> IO (Vector SX)
casadi__Function__sx_out__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sx_out__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sx_out__0 :: FunctionClass a => a -> IO (Vector SX)
function_sx_out__0 x = casadi__Function__sx_out__0 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sx_out__1" c_casadi__Function__sx_out__1
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> IO (Ptr SX')

casadi__Function__sx_out__1
  :: Function -> String -> IO SX
casadi__Function__sx_out__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sx_out__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sx_out__1 :: FunctionClass a => a -> String -> IO SX
function_sx_out__1 x = casadi__Function__sx_out__1 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sx_out__2" c_casadi__Function__sx_out__2
  :: Ptr (Ptr StdString) -> Ptr Function' -> CInt -> IO (Ptr SX')

casadi__Function__sx_out__2
  :: Function -> Int -> IO SX
casadi__Function__sx_out__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sx_out__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
function_sx_out__2 :: FunctionClass a => a -> Int -> IO SX
function_sx_out__2 x = casadi__Function__sx_out__2 (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sz_arg" c_casadi__Function__sz_arg
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CSize

casadi__Function__sz_arg
  :: Function -> IO CSize
casadi__Function__sz_arg x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sz_arg errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sz_arg :: FunctionClass a => a -> IO CSize
function_sz_arg x = casadi__Function__sz_arg (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sz_iw" c_casadi__Function__sz_iw
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CSize

casadi__Function__sz_iw
  :: Function -> IO CSize
casadi__Function__sz_iw x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sz_iw errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sz_iw :: FunctionClass a => a -> IO CSize
function_sz_iw x = casadi__Function__sz_iw (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sz_res" c_casadi__Function__sz_res
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CSize

casadi__Function__sz_res
  :: Function -> IO CSize
casadi__Function__sz_res x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sz_res errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sz_res :: FunctionClass a => a -> IO CSize
function_sz_res x = casadi__Function__sz_res (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__sz_w" c_casadi__Function__sz_w
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO CSize

casadi__Function__sz_w
  :: Function -> IO CSize
casadi__Function__sz_w x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__sz_w errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_sz_w :: FunctionClass a => a -> IO CSize
function_sz_w x = casadi__Function__sz_w (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__type_name" c_casadi__Function__type_name
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr StdString)

casadi__Function__type_name
  :: Function -> IO String
casadi__Function__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_type_name :: FunctionClass a => a -> IO String
function_type_name x = casadi__Function__type_name (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__which_depends" c_casadi__Function__which_depends
  :: Ptr (Ptr StdString) -> Ptr Function' -> Ptr StdString -> Ptr (StdVec (Ptr StdString)) -> CInt -> CInt -> IO (Ptr (StdVec CInt))

casadi__Function__which_depends
  :: Function -> String -> Vector String -> Int -> Bool -> IO (Vector Bool)
casadi__Function__which_depends x0 x1 x2 x3 x4 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3
  x4' <- marshal x4

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__which_depends errStrPtrP x0' x1' x2' x3' x4'
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
function_which_depends :: FunctionClass a => a -> String -> Vector String -> Int -> Bool -> IO (Vector Bool)
function_which_depends x = casadi__Function__which_depends (castFunction x)


-- direct wrapper
foreign import ccall unsafe "casadi__Function__wrap" c_casadi__Function__wrap
  :: Ptr (Ptr StdString) -> Ptr Function' -> IO (Ptr Function')

casadi__Function__wrap
  :: Function -> IO Function
casadi__Function__wrap x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Function__wrap errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
function_wrap :: FunctionClass a => a -> IO Function
function_wrap x = casadi__Function__wrap (castFunction x)

