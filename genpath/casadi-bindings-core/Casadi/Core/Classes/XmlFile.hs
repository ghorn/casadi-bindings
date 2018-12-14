{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.XmlFile
       (
         XmlFile,
         XmlFileClass(..),
         xmlFile__0,
         xmlFile__1,
         xmlFile_doc,
         xmlFile_load_plugin,
         xmlFile_type_name,
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
foreign import ccall unsafe "casadi__XmlFile__CONSTRUCTOR__0" c_casadi__XmlFile__CONSTRUCTOR__0
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr XmlFile')

casadi__XmlFile__CONSTRUCTOR__0
  :: String -> IO XmlFile
casadi__XmlFile__CONSTRUCTOR__0 x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__XmlFile__CONSTRUCTOR__0 errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
xmlFile__0 :: String -> IO XmlFile
xmlFile__0 = casadi__XmlFile__CONSTRUCTOR__0


-- direct wrapper
foreign import ccall unsafe "casadi__XmlFile__CONSTRUCTOR__1" c_casadi__XmlFile__CONSTRUCTOR__1
  :: Ptr (Ptr StdString) -> IO (Ptr XmlFile')

casadi__XmlFile__CONSTRUCTOR__1
  :: IO XmlFile
casadi__XmlFile__CONSTRUCTOR__1  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__XmlFile__CONSTRUCTOR__1 errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
xmlFile__1 :: IO XmlFile
xmlFile__1 = casadi__XmlFile__CONSTRUCTOR__1


-- direct wrapper
foreign import ccall unsafe "casadi__XmlFile__doc" c_casadi__XmlFile__doc
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO (Ptr StdString)

casadi__XmlFile__doc
  :: String -> IO String
casadi__XmlFile__doc x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__XmlFile__doc errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ret



-- classy wrapper
xmlFile_doc :: String -> IO String
xmlFile_doc = casadi__XmlFile__doc


-- direct wrapper
foreign import ccall unsafe "casadi__XmlFile__load_plugin" c_casadi__XmlFile__load_plugin
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

casadi__XmlFile__load_plugin
  :: String -> IO ()
casadi__XmlFile__load_plugin x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__XmlFile__load_plugin errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
xmlFile_load_plugin :: String -> IO ()
xmlFile_load_plugin = casadi__XmlFile__load_plugin


-- direct wrapper
foreign import ccall unsafe "casadi__XmlFile__type_name" c_casadi__XmlFile__type_name
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__XmlFile__type_name
  :: IO String
casadi__XmlFile__type_name  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__XmlFile__type_name errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
xmlFile_type_name :: IO String
xmlFile_type_name = casadi__XmlFile__type_name

