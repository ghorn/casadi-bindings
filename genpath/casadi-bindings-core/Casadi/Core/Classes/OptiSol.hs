{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.OptiSol
       (
         OptiSol,
         OptiSolClass(..),
         optiSol_get_str__0,
         optiSol_get_str__1,
         optiSol_opti,
         optiSol_stats,
         optiSol_type_name,
         optiSol_value__0,
         optiSol_value__1,
         optiSol_value__2,
         optiSol_value__3,
         optiSol_value__4,
         optiSol_value__5,
         optiSol_value_parameters,
         optiSol_value_variables,
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
foreign import ccall unsafe "casadi__OptiSol__get_str__0" c_casadi__OptiSol__get_str__0
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> IO (Ptr StdString)

casadi__OptiSol__get_str__0
  :: OptiSol -> IO String
casadi__OptiSol__get_str__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__get_str__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiSol_get_str__0 :: OptiSolClass a => a -> IO String
optiSol_get_str__0 x = casadi__OptiSol__get_str__0 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__get_str__1" c_casadi__OptiSol__get_str__1
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> CInt -> IO (Ptr StdString)

casadi__OptiSol__get_str__1
  :: OptiSol -> Bool -> IO String
casadi__OptiSol__get_str__1 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__get_str__1 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiSol_get_str__1 :: OptiSolClass a => a -> Bool -> IO String
optiSol_get_str__1 x = casadi__OptiSol__get_str__1 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__opti" c_casadi__OptiSol__opti
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> IO (Ptr Opti')

casadi__OptiSol__opti
  :: OptiSol -> IO Opti
casadi__OptiSol__opti x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__opti errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiSol_opti :: OptiSolClass a => a -> IO Opti
optiSol_opti x = casadi__OptiSol__opti (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__stats" c_casadi__OptiSol__stats
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> IO (Ptr (StdMap StdString (Ptr GenericType')))

casadi__OptiSol__stats
  :: OptiSol -> IO (M.Map String GenericType)
casadi__OptiSol__stats x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__stats errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiSol_stats :: OptiSolClass a => a -> IO (M.Map String GenericType)
optiSol_stats x = casadi__OptiSol__stats (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__type_name" c_casadi__OptiSol__type_name
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> IO (Ptr StdString)

casadi__OptiSol__type_name
  :: OptiSol -> IO String
casadi__OptiSol__type_name x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__type_name errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiSol_type_name :: OptiSolClass a => a -> IO String
optiSol_type_name x = casadi__OptiSol__type_name (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value__0" c_casadi__OptiSol__value__0
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> Ptr SX' -> IO (Ptr DM')

casadi__OptiSol__value__0
  :: OptiSol -> SX -> IO DM
casadi__OptiSol__value__0 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value__0 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiSol_value__0 :: OptiSolClass a => a -> SX -> IO DM
optiSol_value__0 x = casadi__OptiSol__value__0 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value__1" c_casadi__OptiSol__value__1
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> Ptr SX' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr DM')

casadi__OptiSol__value__1
  :: OptiSol -> SX -> Vector MX -> IO DM
casadi__OptiSol__value__1 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value__1 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
optiSol_value__1 :: OptiSolClass a => a -> SX -> Vector MX -> IO DM
optiSol_value__1 x = casadi__OptiSol__value__1 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value__2" c_casadi__OptiSol__value__2
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> Ptr DM' -> IO (Ptr DM')

casadi__OptiSol__value__2
  :: OptiSol -> DM -> IO DM
casadi__OptiSol__value__2 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value__2 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiSol_value__2 :: OptiSolClass a => a -> DM -> IO DM
optiSol_value__2 x = casadi__OptiSol__value__2 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value__3" c_casadi__OptiSol__value__3
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> Ptr DM' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr DM')

casadi__OptiSol__value__3
  :: OptiSol -> DM -> Vector MX -> IO DM
casadi__OptiSol__value__3 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value__3 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
optiSol_value__3 :: OptiSolClass a => a -> DM -> Vector MX -> IO DM
optiSol_value__3 x = casadi__OptiSol__value__3 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value__4" c_casadi__OptiSol__value__4
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> Ptr MX' -> IO (Ptr DM')

casadi__OptiSol__value__4
  :: OptiSol -> MX -> IO DM
casadi__OptiSol__value__4 x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value__4 errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ret



-- classy wrapper
optiSol_value__4 :: OptiSolClass a => a -> MX -> IO DM
optiSol_value__4 x = casadi__OptiSol__value__4 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value__5" c_casadi__OptiSol__value__5
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> Ptr MX' -> Ptr (StdVec (Ptr MX')) -> IO (Ptr DM')

casadi__OptiSol__value__5
  :: OptiSol -> MX -> Vector MX -> IO DM
casadi__OptiSol__value__5 x0 x1 x2 = do
  x0' <- marshal x0
  x1' <- marshal x1
  x2' <- marshal x2

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value__5 errStrPtrP x0' x1' x2'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'
  marshalFree x2 x2'

  return ret



-- classy wrapper
optiSol_value__5 :: OptiSolClass a => a -> MX -> Vector MX -> IO DM
optiSol_value__5 x = casadi__OptiSol__value__5 (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value_parameters" c_casadi__OptiSol__value_parameters
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiSol__value_parameters
  :: OptiSol -> IO (Vector MX)
casadi__OptiSol__value_parameters x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value_parameters errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiSol_value_parameters :: OptiSolClass a => a -> IO (Vector MX)
optiSol_value_parameters x = casadi__OptiSol__value_parameters (castOptiSol x)


-- direct wrapper
foreign import ccall unsafe "casadi__OptiSol__value_variables" c_casadi__OptiSol__value_variables
  :: Ptr (Ptr StdString) -> Ptr OptiSol' -> IO (Ptr (StdVec (Ptr MX')))

casadi__OptiSol__value_variables
  :: OptiSol -> IO (Vector MX)
casadi__OptiSol__value_variables x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiSol__value_variables errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiSol_value_variables :: OptiSolClass a => a -> IO (Vector MX)
optiSol_value_variables x = casadi__OptiSol__value_variables (castOptiSol x)

