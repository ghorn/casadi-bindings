{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.OptiAdvanced
       (
         OptiAdvanced,
         OptiAdvancedClass(..),
         optiAdvanced,
         optiAdvanced_active_symvar,
         optiAdvanced_active_values,
         optiAdvanced_arg,
         optiAdvanced_assert_active_symbol,
         optiAdvanced_assert_baked,
         optiAdvanced_assert_empty,
         optiAdvanced_assert_solved,
         optiAdvanced_bake,
         optiAdvanced_baked_copy,
         optiAdvanced_canon_expr,
         optiAdvanced_casadi_solver,
         optiAdvanced_constraints,
         optiAdvanced_describe__0,
         optiAdvanced_describe__1,
         optiAdvanced_g_describe,
         optiAdvanced_g_lookup,
         optiAdvanced_get_meta,
         optiAdvanced_get_meta_con,
         optiAdvanced_instance_number,
         optiAdvanced_is_parametric,
         optiAdvanced_mark_problem_dirty__0,
         optiAdvanced_mark_problem_dirty__1,
         optiAdvanced_mark_solved__0,
         optiAdvanced_mark_solved__1,
         optiAdvanced_mark_solver_dirty__0,
         optiAdvanced_mark_solver_dirty__1,
         optiAdvanced_objective,
         optiAdvanced_problem_dirty,
         optiAdvanced_res__0,
         optiAdvanced_res__1,
         optiAdvanced_set_meta,
         optiAdvanced_set_meta_con,
         optiAdvanced_show_infeasibilities__0,
         optiAdvanced_show_infeasibilities__1,
         optiAdvanced_solve_actual,
         optiAdvanced_solve_prepare,
         optiAdvanced_solved,
         optiAdvanced_solver_dirty,
         optiAdvanced_symvar__0,
         optiAdvanced_symvar__1,
         optiAdvanced_symvar__2,
         optiAdvanced_x_describe,
         optiAdvanced_x_lookup,
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
foreign import ccall unsafe "casadi__OptiAdvanced__CONSTRUCTOR" c_casadi__OptiAdvanced__CONSTRUCTOR
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr OptiAdvanced')

casadi__OptiAdvanced__CONSTRUCTOR
  :: Opti -> IO OptiAdvanced
casadi__OptiAdvanced__CONSTRUCTOR x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__CONSTRUCTOR errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced :: Opti -> IO OptiAdvanced
optiAdvanced = casadi__OptiAdvanced__CONSTRUCTOR


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__active_symvar" c_casadi__OptiAdvanced__active_symvar
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiAdvanced__active_symvar
  :: OptiAdvanced -> VariableType -> IO (Vector MX)
casadi__OptiAdvanced__active_symvar x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__active_symvar errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_active_symvar :: OptiAdvancedClass a => a -> VariableType -> IO (Vector MX)
optiAdvanced_active_symvar x = casadi__OptiAdvanced__active_symvar (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__active_values" c_casadi__OptiAdvanced__active_values
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CInt -> IO (Ptr (StdVec (Ptr DM')))

casadi__OptiAdvanced__active_values
  :: OptiAdvanced -> VariableType -> IO (Vector DM)
casadi__OptiAdvanced__active_values x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__active_values errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_active_values :: OptiAdvancedClass a => a -> VariableType -> IO (Vector DM)
optiAdvanced_active_values x = casadi__OptiAdvanced__active_values (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__arg" c_casadi__OptiAdvanced__arg
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr (StdMap StdString (Ptr DM')))

casadi__OptiAdvanced__arg
  :: OptiAdvanced -> IO (M.Map String DM)
casadi__OptiAdvanced__arg x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__arg errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_arg :: OptiAdvancedClass a => a -> IO (M.Map String DM)
optiAdvanced_arg x = casadi__OptiAdvanced__arg (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__assert_active_symbol" c_casadi__OptiAdvanced__assert_active_symbol
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO ()

casadi__OptiAdvanced__assert_active_symbol
  :: OptiAdvanced -> MX -> IO ()
casadi__OptiAdvanced__assert_active_symbol x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__assert_active_symbol errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiAdvanced_assert_active_symbol :: OptiAdvancedClass a => a -> MX -> IO ()
optiAdvanced_assert_active_symbol x = casadi__OptiAdvanced__assert_active_symbol (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__assert_baked" c_casadi__OptiAdvanced__assert_baked
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__assert_baked
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__assert_baked x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__assert_baked errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_assert_baked :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_assert_baked x = casadi__OptiAdvanced__assert_baked (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__assert_empty" c_casadi__OptiAdvanced__assert_empty
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__assert_empty
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__assert_empty x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__assert_empty errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_assert_empty :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_assert_empty x = casadi__OptiAdvanced__assert_empty (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__assert_solved" c_casadi__OptiAdvanced__assert_solved
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__assert_solved
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__assert_solved x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__assert_solved errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_assert_solved :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_assert_solved x = casadi__OptiAdvanced__assert_solved (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__bake" c_casadi__OptiAdvanced__bake
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__bake
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__bake x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__bake errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_bake :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_bake x = casadi__OptiAdvanced__bake (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__baked_copy" c_casadi__OptiAdvanced__baked_copy
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr OptiAdvanced')

casadi__OptiAdvanced__baked_copy
  :: OptiAdvanced -> IO OptiAdvanced
casadi__OptiAdvanced__baked_copy x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__baked_copy errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_baked_copy :: OptiAdvancedClass a => a -> IO OptiAdvanced
optiAdvanced_baked_copy x = casadi__OptiAdvanced__baked_copy (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__canon_expr" c_casadi__OptiAdvanced__canon_expr
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO (Ptr MetaCon')

casadi__OptiAdvanced__canon_expr
  :: OptiAdvanced -> MX -> IO MetaCon
casadi__OptiAdvanced__canon_expr x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__canon_expr errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_canon_expr :: OptiAdvancedClass a => a -> MX -> IO MetaCon
optiAdvanced_canon_expr x = casadi__OptiAdvanced__canon_expr (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__casadi_solver" c_casadi__OptiAdvanced__casadi_solver
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr Function')

casadi__OptiAdvanced__casadi_solver
  :: OptiAdvanced -> IO Function
casadi__OptiAdvanced__casadi_solver x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__casadi_solver errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_casadi_solver :: OptiAdvancedClass a => a -> IO Function
optiAdvanced_casadi_solver x = casadi__OptiAdvanced__casadi_solver (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__constraints" c_casadi__OptiAdvanced__constraints
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiAdvanced__constraints
  :: OptiAdvanced -> IO (Vector MX)
casadi__OptiAdvanced__constraints x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__constraints errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_constraints :: OptiAdvancedClass a => a -> IO (Vector MX)
optiAdvanced_constraints x = casadi__OptiAdvanced__constraints (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__describe__0" c_casadi__OptiAdvanced__describe__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO (Ptr StdString)

casadi__OptiAdvanced__describe__0
  :: OptiAdvanced -> MX -> IO String
casadi__OptiAdvanced__describe__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__describe__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_describe__0 :: OptiAdvancedClass a => a -> MX -> IO String
optiAdvanced_describe__0 x = casadi__OptiAdvanced__describe__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__describe__1" c_casadi__OptiAdvanced__describe__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> CLLong -> IO (Ptr StdString)

casadi__OptiAdvanced__describe__1
  :: OptiAdvanced -> MX -> Int -> IO String
casadi__OptiAdvanced__describe__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__describe__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
optiAdvanced_describe__1 :: OptiAdvancedClass a => a -> MX -> Int -> IO String
optiAdvanced_describe__1 x = casadi__OptiAdvanced__describe__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__g_describe" c_casadi__OptiAdvanced__g_describe
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CLLong -> IO (Ptr StdString)

casadi__OptiAdvanced__g_describe
  :: OptiAdvanced -> Int -> IO String
casadi__OptiAdvanced__g_describe x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__g_describe errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_g_describe :: OptiAdvancedClass a => a -> Int -> IO String
optiAdvanced_g_describe x = casadi__OptiAdvanced__g_describe (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__g_lookup" c_casadi__OptiAdvanced__g_lookup
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CLLong -> IO (Ptr MX')

casadi__OptiAdvanced__g_lookup
  :: OptiAdvanced -> Int -> IO MX
casadi__OptiAdvanced__g_lookup x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__g_lookup errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_g_lookup :: OptiAdvancedClass a => a -> Int -> IO MX
optiAdvanced_g_lookup x = casadi__OptiAdvanced__g_lookup (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__get_meta" c_casadi__OptiAdvanced__get_meta
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO (Ptr MetaVar')

casadi__OptiAdvanced__get_meta
  :: OptiAdvanced -> MX -> IO MetaVar
casadi__OptiAdvanced__get_meta x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__get_meta errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_get_meta :: OptiAdvancedClass a => a -> MX -> IO MetaVar
optiAdvanced_get_meta x = casadi__OptiAdvanced__get_meta (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__get_meta_con" c_casadi__OptiAdvanced__get_meta_con
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO (Ptr MetaCon')

casadi__OptiAdvanced__get_meta_con
  :: OptiAdvanced -> MX -> IO MetaCon
casadi__OptiAdvanced__get_meta_con x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__get_meta_con errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_get_meta_con :: OptiAdvancedClass a => a -> MX -> IO MetaCon
optiAdvanced_get_meta_con x = casadi__OptiAdvanced__get_meta_con (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__instance_number" c_casadi__OptiAdvanced__instance_number
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO CLLong

casadi__OptiAdvanced__instance_number
  :: OptiAdvanced -> IO Int
casadi__OptiAdvanced__instance_number x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__instance_number errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_instance_number :: OptiAdvancedClass a => a -> IO Int
optiAdvanced_instance_number x = casadi__OptiAdvanced__instance_number (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__is_parametric" c_casadi__OptiAdvanced__is_parametric
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO CInt

casadi__OptiAdvanced__is_parametric
  :: OptiAdvanced -> MX -> IO Bool
casadi__OptiAdvanced__is_parametric x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__is_parametric errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_is_parametric :: OptiAdvancedClass a => a -> MX -> IO Bool
optiAdvanced_is_parametric x = casadi__OptiAdvanced__is_parametric (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__mark_problem_dirty__0" c_casadi__OptiAdvanced__mark_problem_dirty__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__mark_problem_dirty__0
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__mark_problem_dirty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__mark_problem_dirty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_mark_problem_dirty__0 :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_mark_problem_dirty__0 x = casadi__OptiAdvanced__mark_problem_dirty__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__mark_problem_dirty__1" c_casadi__OptiAdvanced__mark_problem_dirty__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CInt -> IO ()

casadi__OptiAdvanced__mark_problem_dirty__1
  :: OptiAdvanced -> Bool -> IO ()
casadi__OptiAdvanced__mark_problem_dirty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__mark_problem_dirty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiAdvanced_mark_problem_dirty__1 :: OptiAdvancedClass a => a -> Bool -> IO ()
optiAdvanced_mark_problem_dirty__1 x = casadi__OptiAdvanced__mark_problem_dirty__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__mark_solved__0" c_casadi__OptiAdvanced__mark_solved__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__mark_solved__0
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__mark_solved__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__mark_solved__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_mark_solved__0 :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_mark_solved__0 x = casadi__OptiAdvanced__mark_solved__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__mark_solved__1" c_casadi__OptiAdvanced__mark_solved__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CInt -> IO ()

casadi__OptiAdvanced__mark_solved__1
  :: OptiAdvanced -> Bool -> IO ()
casadi__OptiAdvanced__mark_solved__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__mark_solved__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiAdvanced_mark_solved__1 :: OptiAdvancedClass a => a -> Bool -> IO ()
optiAdvanced_mark_solved__1 x = casadi__OptiAdvanced__mark_solved__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__mark_solver_dirty__0" c_casadi__OptiAdvanced__mark_solver_dirty__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__mark_solver_dirty__0
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__mark_solver_dirty__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__mark_solver_dirty__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_mark_solver_dirty__0 :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_mark_solver_dirty__0 x = casadi__OptiAdvanced__mark_solver_dirty__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__mark_solver_dirty__1" c_casadi__OptiAdvanced__mark_solver_dirty__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CInt -> IO ()

casadi__OptiAdvanced__mark_solver_dirty__1
  :: OptiAdvanced -> Bool -> IO ()
casadi__OptiAdvanced__mark_solver_dirty__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__mark_solver_dirty__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiAdvanced_mark_solver_dirty__1 :: OptiAdvancedClass a => a -> Bool -> IO ()
optiAdvanced_mark_solver_dirty__1 x = casadi__OptiAdvanced__mark_solver_dirty__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__objective" c_casadi__OptiAdvanced__objective
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr MX')

casadi__OptiAdvanced__objective
  :: OptiAdvanced -> IO MX
casadi__OptiAdvanced__objective x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__objective errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_objective :: OptiAdvancedClass a => a -> IO MX
optiAdvanced_objective x = casadi__OptiAdvanced__objective (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__problem_dirty" c_casadi__OptiAdvanced__problem_dirty
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO CInt

casadi__OptiAdvanced__problem_dirty
  :: OptiAdvanced -> IO Bool
casadi__OptiAdvanced__problem_dirty x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__problem_dirty errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_problem_dirty :: OptiAdvancedClass a => a -> IO Bool
optiAdvanced_problem_dirty x = casadi__OptiAdvanced__problem_dirty (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__res__0" c_casadi__OptiAdvanced__res__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr (StdMap StdString (Ptr DM')))

casadi__OptiAdvanced__res__0
  :: OptiAdvanced -> IO (M.Map String DM)
casadi__OptiAdvanced__res__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__res__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_res__0 :: OptiAdvancedClass a => a -> IO (M.Map String DM)
optiAdvanced_res__0 x = casadi__OptiAdvanced__res__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__res__1" c_casadi__OptiAdvanced__res__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr (StdMap StdString (Ptr DM')) -> IO ()

casadi__OptiAdvanced__res__1
  :: OptiAdvanced -> M.Map String DM -> IO ()
casadi__OptiAdvanced__res__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__res__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiAdvanced_res__1 :: OptiAdvancedClass a => a -> M.Map String DM -> IO ()
optiAdvanced_res__1 x = casadi__OptiAdvanced__res__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__set_meta" c_casadi__OptiAdvanced__set_meta
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> Ptr MetaVar' -> IO ()

casadi__OptiAdvanced__set_meta
  :: OptiAdvanced -> MX -> MetaVar -> IO ()
casadi__OptiAdvanced__set_meta x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__set_meta errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
optiAdvanced_set_meta :: OptiAdvancedClass a => a -> MX -> MetaVar -> IO ()
optiAdvanced_set_meta x = casadi__OptiAdvanced__set_meta (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__set_meta_con" c_casadi__OptiAdvanced__set_meta_con
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> Ptr MetaCon' -> IO ()

casadi__OptiAdvanced__set_meta_con
  :: OptiAdvanced -> MX -> MetaCon -> IO ()
casadi__OptiAdvanced__set_meta_con x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__set_meta_con errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
optiAdvanced_set_meta_con :: OptiAdvancedClass a => a -> MX -> MetaCon -> IO ()
optiAdvanced_set_meta_con x = casadi__OptiAdvanced__set_meta_con (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__show_infeasibilities__0" c_casadi__OptiAdvanced__show_infeasibilities__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__show_infeasibilities__0
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__show_infeasibilities__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__show_infeasibilities__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_show_infeasibilities__0 :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_show_infeasibilities__0 x = casadi__OptiAdvanced__show_infeasibilities__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__show_infeasibilities__1" c_casadi__OptiAdvanced__show_infeasibilities__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CDouble -> IO ()

casadi__OptiAdvanced__show_infeasibilities__1
  :: OptiAdvanced -> Double -> IO ()
casadi__OptiAdvanced__show_infeasibilities__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__show_infeasibilities__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiAdvanced_show_infeasibilities__1 :: OptiAdvancedClass a => a -> Double -> IO ()
optiAdvanced_show_infeasibilities__1 x = casadi__OptiAdvanced__show_infeasibilities__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__solve_actual" c_casadi__OptiAdvanced__solve_actual
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr (StdMap StdString (Ptr DM')) -> IO (Ptr (StdMap StdString (Ptr DM')))

casadi__OptiAdvanced__solve_actual
  :: OptiAdvanced -> M.Map String DM -> IO (M.Map String DM)
casadi__OptiAdvanced__solve_actual x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__solve_actual errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_solve_actual :: OptiAdvancedClass a => a -> M.Map String DM -> IO (M.Map String DM)
optiAdvanced_solve_actual x = casadi__OptiAdvanced__solve_actual (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__solve_prepare" c_casadi__OptiAdvanced__solve_prepare
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO ()

casadi__OptiAdvanced__solve_prepare
  :: OptiAdvanced -> IO ()
casadi__OptiAdvanced__solve_prepare x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__solve_prepare errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
optiAdvanced_solve_prepare :: OptiAdvancedClass a => a -> IO ()
optiAdvanced_solve_prepare x = casadi__OptiAdvanced__solve_prepare (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__solved" c_casadi__OptiAdvanced__solved
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO CInt

casadi__OptiAdvanced__solved
  :: OptiAdvanced -> IO Bool
casadi__OptiAdvanced__solved x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__solved errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_solved :: OptiAdvancedClass a => a -> IO Bool
optiAdvanced_solved x = casadi__OptiAdvanced__solved (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__solver_dirty" c_casadi__OptiAdvanced__solver_dirty
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO CInt

casadi__OptiAdvanced__solver_dirty
  :: OptiAdvanced -> IO Bool
casadi__OptiAdvanced__solver_dirty x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__solver_dirty errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_solver_dirty :: OptiAdvancedClass a => a -> IO Bool
optiAdvanced_solver_dirty x = casadi__OptiAdvanced__solver_dirty (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__symvar__0" c_casadi__OptiAdvanced__symvar__0
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> CInt -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiAdvanced__symvar__0
  :: OptiAdvanced -> MX -> VariableType -> IO (Vector MX)
casadi__OptiAdvanced__symvar__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__symvar__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
optiAdvanced_symvar__0 :: OptiAdvancedClass a => a -> MX -> VariableType -> IO (Vector MX)
optiAdvanced_symvar__0 x = casadi__OptiAdvanced__symvar__0 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__symvar__1" c_casadi__OptiAdvanced__symvar__1
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> Ptr MX' -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiAdvanced__symvar__1
  :: OptiAdvanced -> MX -> IO (Vector MX)
casadi__OptiAdvanced__symvar__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__symvar__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_symvar__1 :: OptiAdvancedClass a => a -> MX -> IO (Vector MX)
optiAdvanced_symvar__1 x = casadi__OptiAdvanced__symvar__1 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__symvar__2" c_casadi__OptiAdvanced__symvar__2
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiAdvanced__symvar__2
  :: OptiAdvanced -> IO (Vector MX)
casadi__OptiAdvanced__symvar__2 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__symvar__2 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiAdvanced_symvar__2 :: OptiAdvancedClass a => a -> IO (Vector MX)
optiAdvanced_symvar__2 x = casadi__OptiAdvanced__symvar__2 (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__x_describe" c_casadi__OptiAdvanced__x_describe
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CLLong -> IO (Ptr StdString)

casadi__OptiAdvanced__x_describe
  :: OptiAdvanced -> Int -> IO String
casadi__OptiAdvanced__x_describe x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__x_describe errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_x_describe :: OptiAdvancedClass a => a -> Int -> IO String
optiAdvanced_x_describe x = casadi__OptiAdvanced__x_describe (castOptiAdvanced x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiAdvanced__x_lookup" c_casadi__OptiAdvanced__x_lookup
  :: Ptr (Ptr StdString) -> Ptr OptiAdvanced' -> CLLong -> IO (Ptr MX')

casadi__OptiAdvanced__x_lookup
  :: OptiAdvanced -> Int -> IO MX
casadi__OptiAdvanced__x_lookup x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiAdvanced__x_lookup errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiAdvanced_x_lookup :: OptiAdvancedClass a => a -> Int -> IO MX
optiAdvanced_x_lookup x = casadi__OptiAdvanced__x_lookup (castOptiAdvanced x)

