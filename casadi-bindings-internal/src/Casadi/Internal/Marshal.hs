{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language InstanceSigs #-}

module Casadi.Internal.Marshal
       ( Marshal(..)
       , withMarshal
       , newStorableVec
       , HsToC(..)
       , withMarshalStorableVec
       ) where

import Control.Monad ( when )
import qualified Data.Vector as V
import qualified Data.Map as M
import Foreign.C.Types
import Foreign.C.String ( withCString )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal ( withArrayLen )
import Foreign.Storable ( Storable )

import Casadi.Internal.CToolsImports
--import Casadi.Internal.CToolsImports ( c_deleteVecVoidP )
import Casadi.Internal.CppHelpers ( newStdVec, readStdVec, c_newStdString, c_deleteStdString )
import Casadi.Internal.MarshalTypes ( StdMap, StdPair, StdVec, StdString )

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

-- ("int","CInt",Nothing,["Int","Bool"])
instance Marshal (V.Vector CInt) (Ptr (StdVec CInt)) where
  marshal = newStorableVec c_newVecCInt
  marshalFree = const c_deleteVecCInt
instance Marshal (V.Vector Int) (Ptr (StdVec CInt)) where
  marshal = newStorableVec c_newVecCInt . (V.map hsToC)
  marshalFree = const c_deleteVecCInt
instance Marshal (V.Vector Bool) (Ptr (StdVec CInt)) where
  marshal = newStorableVec c_newVecCInt . (V.map hsToC)
  marshalFree = const c_deleteVecCInt

-- ("voidp","VoidP",Just "(Ptr a)",[])
instance Marshal (V.Vector (Ptr a)) (Ptr (StdVec (Ptr a))) where
  marshal = newStorableVec c_newVecVoidP
  marshalFree = const c_deleteVecVoidP

-- ("uchar","CUChar",Nothing,[])
instance Marshal (V.Vector CUChar) (Ptr (StdVec CUChar)) where
  marshal = newStorableVec c_newVecCUChar
  marshalFree = const c_deleteVecCUChar

-- ("double","CDouble",Nothing,["Double"])
instance Marshal (V.Vector CDouble) (Ptr (StdVec CDouble)) where
  marshal = newStorableVec c_newVecCDouble
  marshalFree = const c_deleteVecCDouble
instance Marshal (V.Vector Double) (Ptr (StdVec CDouble)) where
  marshal = newStorableVec c_newVecCDouble . (V.map hsToC)
  marshalFree = const c_deleteVecCDouble

-- stdpair
instance (Marshal a (Ptr pa), Marshal b (Ptr pb))
         => Marshal (a, b) (Ptr (StdPair (Ptr pa) (Ptr pb))) where
  marshal (x,y) = do
    px <- marshal x
    py <- marshal y
    c_newStdPair px py
  marshalFree (x,y) stdpair = do
    px <- c_stdPairFst stdpair
    py <- c_stdPairSnd stdpair
    marshalFree x px
    marshalFree y py
    c_deleteStdPair stdpair

-- stdpair
instance (Marshal a CInt, Marshal b CInt)
         => Marshal (a, b) (Ptr (StdPair CInt CInt)) where
  marshal (x,y) = do
    px <- marshal x
    py <- marshal y
    c_newStdPairInt px py
  marshalFree (x,y) stdpair = do
    cx <- c_stdPairFstInt stdpair
    cy <- c_stdPairSndInt stdpair
    marshalFree x cx
    marshalFree y cy
    c_deleteStdPairInt stdpair

-- stdmap
instance (Marshal a (Ptr pa))
         => Marshal (M.Map String a) (Ptr (StdMap StdString (Ptr pa))) where
  marshal m = do
    let (keys, vals) = unzip (M.toAscList m)
        vecKeys = V.fromList keys
    vecKeyPtrs <- V.mapM marshal vecKeys :: IO (V.Vector (Ptr StdString))
    vecValPtrs <- V.mapM marshal (V.fromList vals) :: IO (V.Vector (Ptr pa))

    stdVecKeys <- newStdVec vecKeyPtrs
                  :: IO (Ptr (StdVec (Ptr StdString)))
    stdVecVals <- newStdVec vecValPtrs
                  :: IO (Ptr (StdVec (Ptr pa)))

    dict <- c_newDict stdVecKeys stdVecVals

    c_deleteVecVoidP stdVecKeys
    c_deleteVecVoidP stdVecVals

    V.zipWithM_ marshalFree vecKeys vecKeyPtrs

    return dict

  marshalFree m dict = do
    let (keys, vals) = unzip (M.toAscList m)
        vecKeys = V.fromList keys
    vecKeyPtrs <- V.mapM marshal vecKeys :: IO (V.Vector (Ptr StdString))
    vecVals <- V.mapM (c_lookupDict dict) vecKeyPtrs :: IO (V.Vector (Ptr pa))

    V.zipWithM_ marshalFree vecKeys vecKeyPtrs
    V.zipWithM_ marshalFree (V.fromList vals) vecVals
    c_deleteDict dict


instance Marshal String (Ptr StdString) where
  marshal str = newStdString str
  marshalFree _ stdStr = c_deleteStdString stdStr

newStdString :: String -> IO (Ptr StdString)
newStdString x = withCString x $ \cstring -> c_newStdString cstring

--instance Marshal String (Ptr CChar) where
--  withMarshal = withCString



instance Marshal a (Ptr b) => Marshal (V.Vector a) (Ptr (StdVec (Ptr b))) where
  marshal vec = do
    ptrs <- V.mapM marshal vec :: IO (V.Vector (Ptr b))
    newStdVec ptrs
  marshalFree vec0 cppvec = do
    ptrs <- readStdVec cppvec :: IO (V.Vector (Ptr b))
    when (V.length vec0 /= V.length ptrs) $
      error "unmarshal: Marshal (Vector a) (Ptr (CooVec (Ptr b))) length mismatch"
    V.zipWithM_ marshalFree vec0 ptrs
    c_deleteVecVoidP cppvec

newStorableVec ::
  Storable a =>
  (Ptr a -> CInt -> IO (Ptr (StdVec a))) ->
  V.Vector a -> IO (Ptr (StdVec a))
newStorableVec newVec vec = do
  withArrayLen (V.toList vec) $ \num array ->
    newVec array (fromIntegral num)

withMarshalStorableVec ::
  Storable a =>
  (Ptr a -> CInt -> IO (Ptr (StdVec a))) ->
  (Ptr (StdVec a) -> IO ()) ->
  V.Vector a -> (Ptr (StdVec a) -> IO b) -> IO b
withMarshalStorableVec newVec deleteVec vec f = do
  ptrStdVec <- withArrayLen (V.toList vec) $ \num array ->
    newVec array (fromIntegral num)
  ret <- f ptrStdVec
  deleteVec ptrStdVec
  return ret
