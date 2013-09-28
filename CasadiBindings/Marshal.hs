{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
-- {-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module CasadiBindings.Marshal ( Marshal(..)
                              , HsToC(..)
                              , CornerCase(..)
                              , withMarshalStorableVec
                              , withMarshalStorableVecVec
                              ) where

import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String ( withCString )
import Foreign.Ptr ( Ptr )
import Foreign.Marshal ( withArray, withArrayLen )
import Foreign.Storable ( Storable )

import CasadiBindings.MarshalTypes
import CasadiBindings.Gen.ForeignToolsImports

class Marshal a b where
  withMarshal :: a -> (b -> IO c) -> IO c

class HsToC a b where
  hsToC :: a -> b
instance HsToC Int CInt where
  hsToC = fromIntegral -- really should check min/max bounds here
instance HsToC Int CLong where
  hsToC = fromIntegral
instance HsToC Bool CInt where
  hsToC True = 1
  hsToC False = 0
instance HsToC Double CDouble where
  hsToC = realToFrac
instance HsToC CUChar CUChar where
  hsToC = id
instance HsToC CSize CSize where
  hsToC = id

instance Marshal Int CInt where
  withMarshal x f = f (hsToC x)
instance Marshal Int CLong where
  withMarshal x f = f (hsToC x)
instance Marshal Bool CInt where
  withMarshal x f = f (hsToC x)
instance Marshal Double CDouble where
  withMarshal x f = f (hsToC x)
instance Marshal CUChar CUChar where
  withMarshal x f = f (hsToC x)
instance Marshal CSize CSize where
  withMarshal x f = f (hsToC x)

foreign import ccall unsafe "hs_new_bool" c_newBool
  :: CInt -> IO (Ptr CppBool')
foreign import ccall unsafe "hs_delete_bool" c_deleteBool
  :: Ptr CppBool' -> IO ()
foreign import ccall unsafe "hs_new_string" c_newStdString
  :: Ptr CChar -> IO (Ptr StdString')
foreign import ccall unsafe "hs_delete_string" c_deleteStdString
  :: Ptr StdString' -> IO ()

instance Marshal Bool (Ptr CppBool') where
  withMarshal x f = do
    boolPtr <- c_newBool (hsToC x)
    ret <- f boolPtr
    c_deleteBool boolPtr
    return ret

instance Marshal String (Ptr CChar) where
  withMarshal = withCString

instance Marshal String (Ptr StdString') where
  withMarshal str f = do
    stdStr <- newStdString str
    ret <- f stdStr
    c_deleteStdString stdStr
    return ret

newStdString :: String -> IO (Ptr StdString')
newStdString x = withCString x $ \cstring -> c_newStdString cstring

newtype CornerCase a = CornerCase a

instance Marshal (CornerCase (V.Vector String)) (Ptr (CppVec (Ptr StdString'))) where
  withMarshal (CornerCase strs) f = do
    stdStrs <- V.mapM newStdString strs
    ret <- withMarshalStorableVec c_newVecVoidP c_deleteVecVoidP stdStrs f
    V.mapM_ c_deleteStdString stdStrs
    return ret
      
instance Marshal (CornerCase (V.Vector Bool)) (Ptr (CppVec (Ptr CppBool'))) where
  withMarshal (CornerCase bools) f = do
    stdBools <- V.mapM (c_newBool . hsToC) bools
    ret <- withMarshalStorableVec c_newVecVoidP c_deleteVecVoidP stdBools f
    V.mapM_ c_deleteBool stdBools
    return ret
      
instance ForeignPtrWrapper a b => Marshal (V.Vector a) (Ptr (CppVec (Ptr b))) where
  withMarshal vec f =
    let foreignPtrList = map unwrapForeignPtr (V.toList vec)
    in withForeignPtrs foreignPtrList $ \ptrList ->
         withMarshalStorableVec c_newVecVoidP c_deleteVecVoidP (V.fromList ptrList) f

instance ForeignPtrWrapper a b => Marshal (V.Vector (V.Vector a)) (Ptr (CppVecVec (Ptr b))) where
  withMarshal vecVec f =
    let foreignVecVec' = V.map (V.map unwrapForeignPtr) vecVec
    in withForeignPtrsVV foreignVecVec' $ \vecVec' ->
         withMarshalStorableVecVec c_newVecVecVoidP c_deleteVecVecVoidP vecVec' f

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

withMarshalStorableVecVec ::
  Storable a =>
  (Ptr a -> CInt -> Ptr CInt -> IO (Ptr (CppVecVec a))) ->
  (Ptr (CppVecVec a) -> IO ()) ->
  V.Vector (V.Vector a) -> (Ptr (CppVecVec a) -> IO b) -> IO b
withMarshalStorableVecVec newVecVec deleteVecVec vecVec f = do
  let vecList = V.toList vecVec
      flatList = concatMap V.toList vecList
      innerLengths = map (fromIntegral . V.length) vecList
      outerLength = length innerLengths
  ptrCppVecVec <- withArray flatList $ \flatArray ->
    withArray innerLengths $ \innerLengthsArray ->
      newVecVec flatArray (fromIntegral outerLength) innerLengthsArray
  ret <- f ptrCppVecVec
  deleteVecVec ptrCppVecVec
  return ret
