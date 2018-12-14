{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.OptiCallback
       (
         OptiCallback,
         OptiCallbackClass(..),
         optiCallback__0,
         optiCallback__1,
         optiCallback_call,
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
foreign import ccall unsafe "casadi__OptiCallback__CONSTRUCTOR__0" c_casadi__OptiCallback__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr OptiCallback' -> IO (Ptr OptiCallback')

casadi__OptiCallback__CONSTRUCTOR__0
  :: OptiCallback -> IO OptiCallback
casadi__OptiCallback__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiCallback__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
optiCallback__0 :: OptiCallback -> IO OptiCallback
optiCallback__0 = casadi__OptiCallback__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__OptiCallback__CONSTRUCTOR__1" c_casadi__OptiCallback__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> IO (Ptr OptiCallback')

casadi__OptiCallback__CONSTRUCTOR__1
  :: IO OptiCallback
casadi__OptiCallback__CONSTRUCTOR__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiCallback__CONSTRUCTOR__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
optiCallback__1 :: IO OptiCallback
optiCallback__1 = casadi__OptiCallback__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__OptiCallback__call" c_casadi__OptiCallback__call
  :: Ptr (Ptr StdString) -> Ptr OptiCallback' -> CLLong -> IO ()

casadi__OptiCallback__call
  :: OptiCallback -> Int -> IO ()
casadi__OptiCallback__call x0 x1 = do
  x0' <- marshal x0
  x1' <- marshal x1

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__OptiCallback__call errStrPtrP x0' x1'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'
  marshalFree x1 x1'

  return ()



-- classy wrapper
optiCallback_call :: OptiCallbackClass a => a -> Int -> IO ()
optiCallback_call x = casadi__OptiCallback__call (castOptiCallback x)

