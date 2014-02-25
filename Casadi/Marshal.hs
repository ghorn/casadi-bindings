{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Casadi.Marshal ( Marshal(..)
                      , withMarshal
                      , newStorableVec
                      , HsToC(..)
                      , withMarshalStorableVec
                      ) where

import Control.Monad ( when )
import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String ( withCString )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal ( withArrayLen )
import Foreign.Storable ( Storable )

import Casadi.Wrappers.CToolsImports ( c_deleteVecVoidP )
import Casadi.CppHelpers ( newCppVec, readCppVec, c_newStdString, c_deleteStdString )
import Casadi.MarshalTypes ( CppVec, StdString' )

class Marshal a b where
  marshal :: a -> IO b
  marshalFree :: a -> b -> IO ()

  marshalFree = const (const (return ()))

withMarshal :: forall a b c. Marshal a b => a -> (b -> IO c) -> IO c
withMarshal x f = do
  x' <- marshal x :: IO b
  ret <- f x' :: IO c
  marshalFree x x' :: IO ()
  return ret :: IO c

class HsToC a b where
  hsToC :: a -> b
instance HsToC Int CInt where
  hsToC = fromIntegral -- really should check min/max bounds here
instance HsToC Int CLong where
  hsToC = fromIntegral
instance HsToC Bool CInt where
  hsToC False = 0
  hsToC True = 1
instance HsToC Double CDouble where
  hsToC = realToFrac
instance HsToC CUChar CUChar where
  hsToC = id
instance HsToC CSize CSize where
  hsToC = id

instance Marshal Int CInt where
  marshal = return . hsToC
instance Marshal Int CLong where
  marshal = return . hsToC
instance Marshal Bool CInt where
  marshal = return . hsToC
instance Marshal Double CDouble where
  marshal = return . hsToC
instance Marshal CUChar CUChar where
  marshal = return . hsToC
instance Marshal CSize CSize where
  marshal = return . hsToC

instance Marshal String (Ptr StdString') where
  marshal str = newStdString str
  marshalFree _ stdStr = c_deleteStdString stdStr

newStdString :: String -> IO (Ptr StdString')
newStdString x = withCString x $ \cstring -> c_newStdString cstring

--instance Marshal String (Ptr CChar) where
--  withMarshal = withCString



instance Marshal a (Ptr b) => Marshal (V.Vector a) (Ptr (CppVec (Ptr b))) where
  marshal vec = do
    ptrs <- V.mapM marshal vec :: IO (V.Vector (Ptr b))
    newCppVec ptrs
  marshalFree vec0 cppvec = do
    ptrs <- readCppVec cppvec :: IO (V.Vector (Ptr b))
    when (V.length vec0 /= V.length ptrs) $
      error "unmarshal: Marshal (Vector a) (Ptr (CooVec (Ptr b))) length mismatch"
    V.zipWithM_ marshalFree vec0 ptrs
    c_deleteVecVoidP cppvec

newStorableVec ::
  Storable a =>
  (Ptr a -> CInt -> IO (Ptr (CppVec a))) ->
  V.Vector a -> IO (Ptr (CppVec a))
newStorableVec newVec vec = do
  withArrayLen (V.toList vec) $ \num array ->
    newVec array (fromIntegral num)

withMarshalStorableVec ::
  Storable a =>
  (Ptr a -> CInt -> IO (Ptr (CppVec a))) ->
  (Ptr (CppVec a) -> IO ()) ->
  V.Vector a -> (Ptr (CppVec a) -> IO b) -> IO b
withMarshalStorableVec newVec deleteVec vec f = do
  ptrCppVec <- withArrayLen (V.toList vec) $ \num array ->
    newVec array (fromIntegral num)
  ret <- f ptrCppVec
  deleteVec ptrCppVec
  return ret
