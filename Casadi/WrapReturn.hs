{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Casadi.WrapReturn ( WrapReturn(..)
                         ) where

import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )
import Foreign.Marshal ( mallocArray, free, peekArray )

import Casadi.MarshalTypes ( CppVec, StdString' )

import Casadi.CppHelpers ( readCppVec, c_lengthStdString, c_copyStdString, c_deleteStdString )
import Casadi.Wrappers.CToolsImports

class WrapReturn a b where
  wrapReturn :: a -> IO b
instance WrapReturn a a where
  wrapReturn = return
instance WrapReturn CInt Int where
  wrapReturn = return . fromIntegral
instance WrapReturn CDouble Double where
  wrapReturn = return . realToFrac
instance WrapReturn CLong Int where
  wrapReturn = return . fromIntegral
instance WrapReturn CInt Bool where
  wrapReturn 0 = return False
  wrapReturn _ = return True


instance WrapReturn (Ptr a) b => WrapReturn (Ptr (CppVec (Ptr a))) (V.Vector b) where
  wrapReturn cppvec = do
    vec <- readCppVec cppvec >>= (V.mapM wrapReturn) :: IO (V.Vector b)
    c_deleteVecVoidP cppvec
    return vec

instance WrapReturn (Ptr StdString') String where
  wrapReturn stdStr = do
    len <- fmap fromIntegral $ c_lengthStdString stdStr
    cstring <- mallocArray (len + 1)
    c_copyStdString stdStr cstring
    ret <- peekCString cstring
    free cstring
    c_deleteStdString stdStr
    return ret

wrapReturnVec ::
  Storable a =>
  (Ptr (CppVec a) -> IO CInt) ->
  (Ptr (CppVec a) -> Ptr a -> IO ()) ->
  (Ptr (CppVec a) -> IO ()) ->
  (a -> IO b) ->
  Ptr (CppVec a) -> IO (V.Vector b)
wrapReturnVec vecSize vecCopy vecDel cToHs vecPtr = do
  n <- fmap fromIntegral (vecSize vecPtr)
  arr <- mallocArray n
  vecCopy vecPtr arr
  ret <- peekArray n arr
  free arr
  vecDel vecPtr
  fmap V.fromList (mapM cToHs ret)

instance WrapReturn (Ptr (CppVec CInt)) (V.Vector Int) where
  wrapReturn = wrapReturnVec c_sizeVecCInt c_copyVecCInt c_deleteVecCInt wrapReturn

instance WrapReturn (Ptr (CppVec CDouble)) (V.Vector Double) where
  wrapReturn = wrapReturnVec c_sizeVecCDouble c_copyVecCDouble c_deleteVecCDouble wrapReturn
