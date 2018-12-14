{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Opti
       (
         Opti,
         OptiClass(..),
         opti,
         opti_advanced,
         opti_bounded,
         opti_callback_class,
         opti_copy,
         opti_debug,
         opti_dual,
         opti_f,
         opti_g,
         opti_get_str__0,
         opti_get_str__1,
         opti_initial,
         opti_lam_g,
         opti_lbg,
         opti_minimize,
         opti_ng,
         opti_np,
         opti_nx,
         opti_p,
         opti_parameter__0,
         opti_parameter__1,
         opti_parameter__2,
         opti_parameter__3,
         opti_return_status,
         opti_set_initial__0,
         opti_set_initial__1,
         opti_set_value__0,
         opti_set_value__1,
         opti_solve,
         opti_solver__0,
         opti_solver__1,
         opti_solver__2,
         opti_stats,
         opti_subject_to__0,
         opti_subject_to__1,
         opti_subject_to__2,
         opti_type_name,
         opti_ubg,
         opti_update_user_dict__0,
         opti_update_user_dict__1,
         opti_user_dict,
         opti_value__0,
         opti_value__1,
         opti_value__2,
         opti_value__3,
         opti_value__4,
         opti_value__5,
         opti_value_parameters,
         opti_value_variables,
         opti_variable__0,
         opti_variable__1,
         opti_variable__2,
         opti_variable__3,
         opti_x,
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
foreign import ccall unsafe "casadi__Opti__CONSTRUCTOR" c_casadi__Opti__CONSTRUCTOR
  :: Ptr (Ptr StdString) -> IO (Ptr Opti')

casadi__Opti__CONSTRUCTOR
  :: IO Opti
casadi__Opti__CONSTRUCTOR  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__CONSTRUCTOR errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
opti :: IO Opti
opti = casadi__Opti__CONSTRUCTOR


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__advanced" c_casadi__Opti__advanced
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr OptiAdvanced')

casadi__Opti__advanced
  :: Opti -> IO OptiAdvanced
casadi__Opti__advanced x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__advanced errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_advanced :: OptiClass a => a -> IO OptiAdvanced
opti_advanced x = casadi__Opti__advanced (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__bounded" c_casadi__Opti__bounded
  :: Ptr (Ptr StdString) -> Ptr MX' -> Ptr MX' -> Ptr MX' -> IO (Ptr MX')

casadi__Opti__bounded
  :: MX -> MX -> MX -> IO MX
casadi__Opti__bounded x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__bounded errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
opti_bounded :: MX -> MX -> MX -> IO MX
opti_bounded = casadi__Opti__bounded


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__callback_class" c_casadi__Opti__callback_class
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO ()

casadi__Opti__callback_class
  :: Opti -> IO ()
casadi__Opti__callback_class x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__callback_class errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
opti_callback_class :: OptiClass a => a -> IO ()
opti_callback_class x = casadi__Opti__callback_class (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__copy" c_casadi__Opti__copy
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr Opti')

casadi__Opti__copy
  :: Opti -> IO Opti
casadi__Opti__copy x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__copy errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_copy :: OptiClass a => a -> IO Opti
opti_copy x = casadi__Opti__copy (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__debug" c_casadi__Opti__debug
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr OptiAdvanced')

casadi__Opti__debug
  :: Opti -> IO OptiAdvanced
casadi__Opti__debug x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__debug errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_debug :: OptiClass a => a -> IO OptiAdvanced
opti_debug x = casadi__Opti__debug (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__dual" c_casadi__Opti__dual
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> IO (Ptr MX')

casadi__Opti__dual
  :: Opti -> MX -> IO MX
casadi__Opti__dual x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__dual errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_dual :: OptiClass a => a -> MX -> IO MX
opti_dual x = casadi__Opti__dual (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__f" c_casadi__Opti__f
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__f
  :: Opti -> IO MX
casadi__Opti__f x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__f errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_f :: OptiClass a => a -> IO MX
opti_f x = casadi__Opti__f (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__g" c_casadi__Opti__g
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__g
  :: Opti -> IO MX
casadi__Opti__g x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__g errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_g :: OptiClass a => a -> IO MX
opti_g x = casadi__Opti__g (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__get_str__0" c_casadi__Opti__get_str__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr StdString)

casadi__Opti__get_str__0
  :: Opti -> IO String
casadi__Opti__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_get_str__0 :: OptiClass a => a -> IO String
opti_get_str__0 x = casadi__Opti__get_str__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__get_str__1" c_casadi__Opti__get_str__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CInt -> IO (Ptr StdString)

casadi__Opti__get_str__1
  :: Opti -> Bool -> IO String
casadi__Opti__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_get_str__1 :: OptiClass a => a -> Bool -> IO String
opti_get_str__1 x = casadi__Opti__get_str__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__initial" c_casadi__Opti__initial
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr (StdVec (Ptr MX')))

casadi__Opti__initial
  :: Opti -> IO (Vector MX)
casadi__Opti__initial x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__initial errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_initial :: OptiClass a => a -> IO (Vector MX)
opti_initial x = casadi__Opti__initial (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__lam_g" c_casadi__Opti__lam_g
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__lam_g
  :: Opti -> IO MX
casadi__Opti__lam_g x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__lam_g errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_lam_g :: OptiClass a => a -> IO MX
opti_lam_g x = casadi__Opti__lam_g (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__lbg" c_casadi__Opti__lbg
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__lbg
  :: Opti -> IO MX
casadi__Opti__lbg x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__lbg errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_lbg :: OptiClass a => a -> IO MX
opti_lbg x = casadi__Opti__lbg (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__minimize" c_casadi__Opti__minimize
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> IO ()

casadi__Opti__minimize
  :: Opti -> MX -> IO ()
casadi__Opti__minimize x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__minimize errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
opti_minimize :: OptiClass a => a -> MX -> IO ()
opti_minimize x = casadi__Opti__minimize (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__ng" c_casadi__Opti__ng
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO CLLong

casadi__Opti__ng
  :: Opti -> IO Int
casadi__Opti__ng x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__ng errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_ng :: OptiClass a => a -> IO Int
opti_ng x = casadi__Opti__ng (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__np" c_casadi__Opti__np
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO CLLong

casadi__Opti__np
  :: Opti -> IO Int
casadi__Opti__np x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__np errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_np :: OptiClass a => a -> IO Int
opti_np x = casadi__Opti__np (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__nx" c_casadi__Opti__nx
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO CLLong

casadi__Opti__nx
  :: Opti -> IO Int
casadi__Opti__nx x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__nx errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_nx :: OptiClass a => a -> IO Int
opti_nx x = casadi__Opti__nx (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__p" c_casadi__Opti__p
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__p
  :: Opti -> IO MX
casadi__Opti__p x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__p errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_p :: OptiClass a => a -> IO MX
opti_p x = casadi__Opti__p (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__parameter__0" c_casadi__Opti__parameter__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__parameter__0
  :: Opti -> IO MX
casadi__Opti__parameter__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__parameter__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_parameter__0 :: OptiClass a => a -> IO MX
opti_parameter__0 x = casadi__Opti__parameter__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__parameter__1" c_casadi__Opti__parameter__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CLLong -> IO (Ptr MX')

casadi__Opti__parameter__1
  :: Opti -> Int -> IO MX
casadi__Opti__parameter__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__parameter__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_parameter__1 :: OptiClass a => a -> Int -> IO MX
opti_parameter__1 x = casadi__Opti__parameter__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__parameter__2" c_casadi__Opti__parameter__2
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CLLong -> CLLong -> IO (Ptr MX')

casadi__Opti__parameter__2
  :: Opti -> Int -> Int -> IO MX
casadi__Opti__parameter__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__parameter__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
opti_parameter__2 :: OptiClass a => a -> Int -> Int -> IO MX
opti_parameter__2 x = casadi__Opti__parameter__2 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__parameter__3" c_casadi__Opti__parameter__3
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CLLong -> CLLong -> Ptr StdString -> IO (Ptr MX')

casadi__Opti__parameter__3
  :: Opti -> Int -> Int -> String -> IO MX
casadi__Opti__parameter__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__parameter__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
opti_parameter__3 :: OptiClass a => a -> Int -> Int -> String -> IO MX
opti_parameter__3 x = casadi__Opti__parameter__3 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__return_status" c_casadi__Opti__return_status
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr StdString)

casadi__Opti__return_status
  :: Opti -> IO String
casadi__Opti__return_status x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__return_status errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_return_status :: OptiClass a => a -> IO String
opti_return_status x = casadi__Opti__return_status (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__set_initial__0" c_casadi__Opti__set_initial__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr (StdVec (Ptr MX')) -> IO ()

casadi__Opti__set_initial__0
  :: Opti -> Vector MX -> IO ()
casadi__Opti__set_initial__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__set_initial__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
opti_set_initial__0 :: OptiClass a => a -> Vector MX -> IO ()
opti_set_initial__0 x = casadi__Opti__set_initial__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__set_initial__1" c_casadi__Opti__set_initial__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> Ptr DM' -> IO ()

casadi__Opti__set_initial__1
  :: Opti -> MX -> DM -> IO ()
casadi__Opti__set_initial__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__set_initial__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
opti_set_initial__1 :: OptiClass a => a -> MX -> DM -> IO ()
opti_set_initial__1 x = casadi__Opti__set_initial__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__set_value__0" c_casadi__Opti__set_value__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr (StdVec (Ptr MX')) -> IO ()

casadi__Opti__set_value__0
  :: Opti -> Vector MX -> IO ()
casadi__Opti__set_value__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__set_value__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
opti_set_value__0 :: OptiClass a => a -> Vector MX -> IO ()
opti_set_value__0 x = casadi__Opti__set_value__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__set_value__1" c_casadi__Opti__set_value__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> Ptr DM' -> IO ()

casadi__Opti__set_value__1
  :: Opti -> MX -> DM -> IO ()
casadi__Opti__set_value__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__set_value__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
opti_set_value__1 :: OptiClass a => a -> MX -> DM -> IO ()
opti_set_value__1 x = casadi__Opti__set_value__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__solve" c_casadi__Opti__solve
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr OptiSol')

casadi__Opti__solve
  :: Opti -> IO OptiSol
casadi__Opti__solve x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__solve errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_solve :: OptiClass a => a -> IO OptiSol
opti_solve x = casadi__Opti__solve (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__solver__0" c_casadi__Opti__solver__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr StdString -> IO ()

casadi__Opti__solver__0
  :: Opti -> String -> IO ()
casadi__Opti__solver__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__solver__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
opti_solver__0 :: OptiClass a => a -> String -> IO ()
opti_solver__0 x = casadi__Opti__solver__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__solver__1" c_casadi__Opti__solver__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> IO ()

casadi__Opti__solver__1
  :: Opti -> String -> M.Map String GenericType -> IO ()
casadi__Opti__solver__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__solver__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
opti_solver__1 :: OptiClass a => a -> String -> M.Map String GenericType -> IO ()
opti_solver__1 x = casadi__Opti__solver__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__solver__2" c_casadi__Opti__solver__2
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr StdString -> Ptr (StdMap StdString (Ptr GenericType')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO ()

casadi__Opti__solver__2
  :: Opti -> String -> M.Map String GenericType -> M.Map String GenericType -> IO ()
casadi__Opti__solver__2 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__solver__2 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ()



-- classy wrapper
opti_solver__2 :: OptiClass a => a -> String -> M.Map String GenericType -> M.Map String GenericType -> IO ()
opti_solver__2 x = casadi__Opti__solver__2 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__stats" c_casadi__Opti__stats
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__Opti__stats
  :: Opti -> IO (M.Map String GenericType)
casadi__Opti__stats x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__stats errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_stats :: OptiClass a => a -> IO (M.Map String GenericType)
opti_stats x = casadi__Opti__stats (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__subject_to__0" c_casadi__Opti__subject_to__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO ()

casadi__Opti__subject_to__0
  :: Opti -> IO ()
casadi__Opti__subject_to__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__subject_to__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
opti_subject_to__0 :: OptiClass a => a -> IO ()
opti_subject_to__0 x = casadi__Opti__subject_to__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__subject_to__1" c_casadi__Opti__subject_to__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr (StdVec (Ptr MX')) -> IO ()

casadi__Opti__subject_to__1
  :: Opti -> Vector MX -> IO ()
casadi__Opti__subject_to__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__subject_to__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
opti_subject_to__1 :: OptiClass a => a -> Vector MX -> IO ()
opti_subject_to__1 x = casadi__Opti__subject_to__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__subject_to__2" c_casadi__Opti__subject_to__2
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> IO ()

casadi__Opti__subject_to__2
  :: Opti -> MX -> IO ()
casadi__Opti__subject_to__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__subject_to__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
opti_subject_to__2 :: OptiClass a => a -> MX -> IO ()
opti_subject_to__2 x = casadi__Opti__subject_to__2 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__type_name" c_casadi__Opti__type_name
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr StdString)

casadi__Opti__type_name
  :: Opti -> IO String
casadi__Opti__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_type_name :: OptiClass a => a -> IO String
opti_type_name x = casadi__Opti__type_name (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__ubg" c_casadi__Opti__ubg
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__ubg
  :: Opti -> IO MX
casadi__Opti__ubg x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__ubg errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_ubg :: OptiClass a => a -> IO MX
opti_ubg x = casadi__Opti__ubg (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__update_user_dict__0" c_casadi__Opti__update_user_dict__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr (StdVec (Ptr MX')) -> Ptr (StdMap StdString (Ptr GenericType')) -> IO ()

casadi__Opti__update_user_dict__0
  :: Opti -> Vector MX -> M.Map String GenericType -> IO ()
casadi__Opti__update_user_dict__0 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__update_user_dict__0 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
opti_update_user_dict__0 :: OptiClass a => a -> Vector MX -> M.Map String GenericType -> IO ()
opti_update_user_dict__0 x = casadi__Opti__update_user_dict__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__update_user_dict__1" c_casadi__Opti__update_user_dict__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> Ptr (StdMap StdString (Ptr GenericType')) -> IO ()

casadi__Opti__update_user_dict__1
  :: Opti -> MX -> M.Map String GenericType -> IO ()
casadi__Opti__update_user_dict__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__update_user_dict__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ()



-- classy wrapper
opti_update_user_dict__1 :: OptiClass a => a -> MX -> M.Map String GenericType -> IO ()
opti_update_user_dict__1 x = casadi__Opti__update_user_dict__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__user_dict" c_casadi__Opti__user_dict
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__Opti__user_dict
  :: Opti -> MX -> IO (M.Map String GenericType)
casadi__Opti__user_dict x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__user_dict errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_user_dict :: OptiClass a => a -> MX -> IO (M.Map String GenericType)
opti_user_dict x = casadi__Opti__user_dict (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value__0" c_casadi__Opti__value__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr SX' -> IO (Ptr DM')

casadi__Opti__value__0
  :: Opti -> SX -> IO DM
casadi__Opti__value__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_value__0 :: OptiClass a => a -> SX -> IO DM
opti_value__0 x = casadi__Opti__value__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value__1" c_casadi__Opti__value__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr SX' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr DM')

casadi__Opti__value__1
  :: Opti -> SX -> Vector MX -> IO DM
casadi__Opti__value__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
opti_value__1 :: OptiClass a => a -> SX -> Vector MX -> IO DM
opti_value__1 x = casadi__Opti__value__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value__2" c_casadi__Opti__value__2
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr DM' -> IO (Ptr DM')

casadi__Opti__value__2
  :: Opti -> DM -> IO DM
casadi__Opti__value__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_value__2 :: OptiClass a => a -> DM -> IO DM
opti_value__2 x = casadi__Opti__value__2 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value__3" c_casadi__Opti__value__3
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr DM' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr DM')

casadi__Opti__value__3
  :: Opti -> DM -> Vector MX -> IO DM
casadi__Opti__value__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
opti_value__3 :: OptiClass a => a -> DM -> Vector MX -> IO DM
opti_value__3 x = casadi__Opti__value__3 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value__4" c_casadi__Opti__value__4
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> IO (Ptr DM')

casadi__Opti__value__4
  :: Opti -> MX -> IO DM
casadi__Opti__value__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_value__4 :: OptiClass a => a -> MX -> IO DM
opti_value__4 x = casadi__Opti__value__4 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value__5" c_casadi__Opti__value__5
  :: Ptr (Ptr StdString) -> Ptr Opti' -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr DM')

casadi__Opti__value__5
  :: Opti -> MX -> Vector MX -> IO DM
casadi__Opti__value__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
opti_value__5 :: OptiClass a => a -> MX -> Vector MX -> IO DM
opti_value__5 x = casadi__Opti__value__5 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value_parameters" c_casadi__Opti__value_parameters
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr (StdVec (Ptr MX')))

casadi__Opti__value_parameters
  :: Opti -> IO (Vector MX)
casadi__Opti__value_parameters x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value_parameters errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_value_parameters :: OptiClass a => a -> IO (Vector MX)
opti_value_parameters x = casadi__Opti__value_parameters (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__value_variables" c_casadi__Opti__value_variables
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr (StdVec (Ptr MX')))

casadi__Opti__value_variables
  :: Opti -> IO (Vector MX)
casadi__Opti__value_variables x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__value_variables errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_value_variables :: OptiClass a => a -> IO (Vector MX)
opti_value_variables x = casadi__Opti__value_variables (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__variable__0" c_casadi__Opti__variable__0
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__variable__0
  :: Opti -> IO MX
casadi__Opti__variable__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__variable__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_variable__0 :: OptiClass a => a -> IO MX
opti_variable__0 x = casadi__Opti__variable__0 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__variable__1" c_casadi__Opti__variable__1
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CLLong -> IO (Ptr MX')

casadi__Opti__variable__1
  :: Opti -> Int -> IO MX
casadi__Opti__variable__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__variable__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
opti_variable__1 :: OptiClass a => a -> Int -> IO MX
opti_variable__1 x = casadi__Opti__variable__1 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__variable__2" c_casadi__Opti__variable__2
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CLLong -> CLLong -> IO (Ptr MX')

casadi__Opti__variable__2
  :: Opti -> Int -> Int -> IO MX
casadi__Opti__variable__2 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__variable__2 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
opti_variable__2 :: OptiClass a => a -> Int -> Int -> IO MX
opti_variable__2 x = casadi__Opti__variable__2 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__variable__3" c_casadi__Opti__variable__3
  :: Ptr (Ptr StdString) -> Ptr Opti' -> CLLong -> CLLong -> Ptr StdString -> IO (Ptr MX')

casadi__Opti__variable__3
  :: Opti -> Int -> Int -> String -> IO MX
casadi__Opti__variable__3 x0 x1 x2 x3 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2
  x3' <- marshal x3

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__variable__3 errStrPtrP x0' x1' x2' x3'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'
  marshalFree x3 x3'

  return ret



-- classy wrapper
opti_variable__3 :: OptiClass a => a -> Int -> Int -> String -> IO MX
opti_variable__3 x = casadi__Opti__variable__3 (castOpti x)


-- direct wrapper
foreign import ccall unsafe "casadi__Opti__x" c_casadi__Opti__x
  :: Ptr (Ptr StdString) -> Ptr Opti' -> IO (Ptr MX')

casadi__Opti__x
  :: Opti -> IO MX
casadi__Opti__x x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Opti__x errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
opti_x :: OptiClass a => a -> IO MX
opti_x x = casadi__Opti__x (castOpti x)

