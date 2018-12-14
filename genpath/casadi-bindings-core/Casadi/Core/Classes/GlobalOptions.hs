{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language ForeignFunctionInterface #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Casadi.Core.Classes.GlobalOptions
       (
         GlobalOptions,
         GlobalOptionsClass(..),
         globalOptions_getCasadiPath,
         globalOptions_getHierarchicalSparsity,
         globalOptions_getMaxNumDir,
         globalOptions_getSimplificationOnTheFly,
         globalOptions_setCasadiPath,
         globalOptions_setHierarchicalSparsity,
         globalOptions_setMaxNumDir,
         globalOptions_setSimplificationOnTheFly,
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
foreign import ccall unsafe "casadi__GlobalOptions__getCasadiPath" c_casadi__GlobalOptions__getCasadiPath
  :: Ptr (Ptr StdString) -> IO (Ptr StdString)

casadi__GlobalOptions__getCasadiPath
  :: IO String
casadi__GlobalOptions__getCasadiPath  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__getCasadiPath errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
globalOptions_getCasadiPath :: IO String
globalOptions_getCasadiPath = casadi__GlobalOptions__getCasadiPath


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__getHierarchicalSparsity" c_casadi__GlobalOptions__getHierarchicalSparsity
  :: Ptr (Ptr StdString) -> IO CInt

casadi__GlobalOptions__getHierarchicalSparsity
  :: IO Bool
casadi__GlobalOptions__getHierarchicalSparsity  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__getHierarchicalSparsity errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
globalOptions_getHierarchicalSparsity :: IO Bool
globalOptions_getHierarchicalSparsity = casadi__GlobalOptions__getHierarchicalSparsity


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__getMaxNumDir" c_casadi__GlobalOptions__getMaxNumDir
  :: Ptr (Ptr StdString) -> IO CLLong

casadi__GlobalOptions__getMaxNumDir
  :: IO Int
casadi__GlobalOptions__getMaxNumDir  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__getMaxNumDir errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
globalOptions_getMaxNumDir :: IO Int
globalOptions_getMaxNumDir = casadi__GlobalOptions__getMaxNumDir


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__getSimplificationOnTheFly" c_casadi__GlobalOptions__getSimplificationOnTheFly
  :: Ptr (Ptr StdString) -> IO CInt

casadi__GlobalOptions__getSimplificationOnTheFly
  :: IO Bool
casadi__GlobalOptions__getSimplificationOnTheFly  = do


  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__getSimplificationOnTheFly errStrPtrP 
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  ret <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)



  return ret



-- classy wrapper
globalOptions_getSimplificationOnTheFly :: IO Bool
globalOptions_getSimplificationOnTheFly = casadi__GlobalOptions__getSimplificationOnTheFly


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__setCasadiPath" c_casadi__GlobalOptions__setCasadiPath
  :: Ptr (Ptr StdString) -> Ptr StdString -> IO ()

casadi__GlobalOptions__setCasadiPath
  :: String -> IO ()
casadi__GlobalOptions__setCasadiPath x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__setCasadiPath errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
globalOptions_setCasadiPath :: String -> IO ()
globalOptions_setCasadiPath = casadi__GlobalOptions__setCasadiPath


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__setHierarchicalSparsity" c_casadi__GlobalOptions__setHierarchicalSparsity
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__GlobalOptions__setHierarchicalSparsity
  :: Bool -> IO ()
casadi__GlobalOptions__setHierarchicalSparsity x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__setHierarchicalSparsity errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
globalOptions_setHierarchicalSparsity :: Bool -> IO ()
globalOptions_setHierarchicalSparsity = casadi__GlobalOptions__setHierarchicalSparsity


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__setMaxNumDir" c_casadi__GlobalOptions__setMaxNumDir
  :: Ptr (Ptr StdString) -> CLLong -> IO ()

casadi__GlobalOptions__setMaxNumDir
  :: Int -> IO ()
casadi__GlobalOptions__setMaxNumDir x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__setMaxNumDir errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
globalOptions_setMaxNumDir :: Int -> IO ()
globalOptions_setMaxNumDir = casadi__GlobalOptions__setMaxNumDir


-- direct wrapper
foreign import ccall unsafe "casadi__GlobalOptions__setSimplificationOnTheFly" c_casadi__GlobalOptions__setSimplificationOnTheFly
  :: Ptr (Ptr StdString) -> CInt -> IO ()

casadi__GlobalOptions__setSimplificationOnTheFly
  :: Bool -> IO ()
casadi__GlobalOptions__setSimplificationOnTheFly x0 = do
  x0' <- marshal x0

  errStrPtrP <- new nullPtr
  ret0 <- c_casadi__GlobalOptions__setSimplificationOnTheFly errStrPtrP x0'
  errStrPtr <- peek errStrPtrP
  free errStrPtrP

  () <- if errStrPtr == nullPtr then wrapReturn ret0 else wrapReturn errStrPtr >>= (error . formatException)

  marshalFree x0 x0'

  return ()



-- classy wrapper
globalOptions_setSimplificationOnTheFly :: Bool -> IO ()
globalOptions_setSimplificationOnTheFly = casadi__GlobalOptions__setSimplificationOnTheFly

