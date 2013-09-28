{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
-- {-# Language FunctionalDependencies #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module CasadiBindings.HsToC ( HsToC(..)
                            ) where

import qualified Data.Vector as V
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import Foreign.Marshal -- ( mallocArray )
import Foreign.Storable ( Storable )

import CasadiBindings.ForeignTools
import CasadiBindings.Gen.ForeignTools
import CasadiBindings.MarshalTypes
import CasadiBindings.Marshal



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

instance Marshal Bool (Ptr CppBool') where
  withMarshal x f = do
    let x' = case x of False -> 0
                       True -> 1
    boolPtr <- c_newBool x'
    ret <- f boolPtr
    c_deleteBool boolPtr
    return ret

instance Marshal String (Ptr CChar) where
  withMarshal = withCString

instance Marshal String (Ptr StdString') where
  withMarshal str f =
    withCString str $ \cstring -> do
      stdStr <- c_newStdString cstring
      ret <- f stdStr
      c_deleteStdString stdStr
      return ret
      
instance Marshal (ForeignPtr a) (Ptr a) where
  withMarshal = withForeignPtr

instance (HsToC a CDouble, Marshal CDouble c) => Marshal (V.Vector a) (Ptr (CppVec (Ptr c))) where
  withMarshal x = withMarshal (V.map hsToC x)
---------- PUT ME BACK ---------------
--instance ForeignPtrWrapper a b => Marshal (V.Vector a) (Ptr (CppVec (Ptr b))) where
--  withMarshal vec f = do
--    let foreignPtrList = map unwrapForeignPtr (V.toList vec)
--    withForeignPtrs foreignPtrList $ \ptrList -> do
--      ptrCppVec <- withArrayLen ptrList $ \num ptrArray ->
--        c_newVecVoidP ptrArray (fromIntegral num)
--      ret <- f ptrCppVec
--      c_deleteVecVoidP ptrCppVec
--      return ret
--
--instance ForeignPtrWrapper a b => Marshal (V.Vector (V.Vector a)) (Ptr (CppVecVec (Ptr b))) where
--  withMarshal vec f = do
--    let innerLengths = map (fromIntegral . V.length) (V.toList vec)
--        outerLength = length innerLengths
--        foreignPtrList = concatMap (map unwrapForeignPtr . V.toList) (V.toList vec)
--    withForeignPtrs foreignPtrList $ \ptrList -> do
--      withArray ptrList $ \ptrArray -> do
--        ptrCppVecVec <- withArray innerLengths $ \innerLengthsArray ->
--          c_newVecVecVoidP ptrArray (fromIntegral outerLength) innerLengthsArray
--        ret <- f ptrCppVecVec
--        c_deleteVecVecVoidP ptrCppVecVec
--        return ret





--instance Marshal (V.Vector CUChar) (Ptr (CppVec CUChar)) where
--  withMarshal = withMarshalStorableVec c_newVecCUChar c_deleteVecCUChar
--instance Marshal (V.Vector Double) (Ptr (CppVec CDouble)) where
--  withMarshal x f = withMarshalStorableVec c_newVecCDouble c_deleteVecCDouble x' f
--    where
--      x' = V.map realToFrac x
--instance Marshal (V.Vector CInt) (Ptr (CppVec CInt)) where
--  withMarshal = withMarshalStorableVec c_hsMarshalVecInt c_hsDeleteVecInt
--instance Marshal (V.Vector CSize) (Ptr (CppVec CSize)) where
--  withMarshal = withMarshalStorableVec c_hsMarshalVecSize c_hsDeleteVecSize
--instance Marshal (Vector Double) (Ptr (CppVec CDouble)))

withMarshalStorableVec ::
  Storable a =>
  (Ptr a -> CInt -> IO (Ptr (CppVec a))) ->
  (Ptr (CppVec a) -> IO ()) ->
  (V.Vector a) -> (Ptr (CppVec a) -> IO b) -> IO b
withMarshalStorableVec hsMarshalVec hsDeleteVec vec f = do
  withArrayLen ((V.toList vec)) $ \num array -> do
    ptrCppVec <- hsMarshalVec array (fromIntegral num)
    ret <- f ptrCppVec
    hsDeleteVec ptrCppVec
    return ret
