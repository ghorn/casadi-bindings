{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.Variable
       (
         Variable,
         VariableClass(..),
         variable__0,
         variable__1,
         variable__2,
         variable_get_str__0,
         variable_get_str__1,
         variable_name,
         variable_type_name,
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
foreign import ccall unsafe "casadi__Variable__CONSTRUCTOR__0" c_casadi__Variable__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr Variable')

casadi__Variable__CONSTRUCTOR__0
  :: String -> IO Variable
casadi__Variable__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
variable__0 :: String -> IO Variable
variable__0 = casadi__Variable__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__Variable__CONSTRUCTOR__1" c_casadi__Variable__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> Ptr StdString -> Ptr Sparsity' -> IO (Ptr Variable')

casadi__Variable__CONSTRUCTOR__1
  :: String -> Sparsity -> IO Variable
casadi__Variable__CONSTRUCTOR__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__CONSTRUCTOR__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
variable__1 :: String -> Sparsity -> IO Variable
variable__1 = casadi__Variable__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__Variable__CONSTRUCTOR__2" c_casadi__Variable__CONSTRUCTOR__2
  :: Ptr (Ptr StdString) -> IO (Ptr Variable')

casadi__Variable__CONSTRUCTOR__2
  :: IO Variable
casadi__Variable__CONSTRUCTOR__2  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__CONSTRUCTOR__2 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
variable__2 :: IO Variable
variable__2 = casadi__Variable__CONSTRUCTOR__2


-- direct wrapper
foreign import ccall unsafe "casadi__Variable__get_str__0" c_casadi__Variable__get_str__0
  :: Ptr (Ptr StdString) -> Ptr Variable' -> IO (Ptr StdString)

casadi__Variable__get_str__0
  :: Variable -> IO String
casadi__Variable__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
variable_get_str__0 :: VariableClass a => a -> IO String
variable_get_str__0 x = casadi__Variable__get_str__0 (castVariable x)


-- direct wrapper
foreign import ccall unsafe "casadi__Variable__get_str__1" c_casadi__Variable__get_str__1
  :: Ptr (Ptr StdString) -> Ptr Variable' -> CInt -> IO (Ptr StdString)

casadi__Variable__get_str__1
  :: Variable -> Bool -> IO String
casadi__Variable__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
variable_get_str__1 :: VariableClass a => a -> Bool -> IO String
variable_get_str__1 x = casadi__Variable__get_str__1 (castVariable x)


-- direct wrapper
foreign import ccall unsafe "casadi__Variable__name" c_casadi__Variable__name
  :: Ptr (Ptr StdString) -> Ptr Variable' -> IO (Ptr StdString)

casadi__Variable__name
  :: Variable -> IO String
casadi__Variable__name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
variable_name :: VariableClass a => a -> IO String
variable_name x = casadi__Variable__name (castVariable x)


-- direct wrapper
foreign import ccall unsafe "casadi__Variable__type_name" c_casadi__Variable__type_name
  :: Ptr (Ptr StdString) -> Ptr Variable' -> IO (Ptr StdString)

casadi__Variable__type_name
  :: Variable -> IO String
casadi__Variable__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__Variable__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
variable_type_name :: VariableClass a => a -> IO String
variable_type_name x = casadi__Variable__type_name (castVariable x)

