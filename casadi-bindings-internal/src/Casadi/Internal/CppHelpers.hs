{-# OPTIONS_GHC -Wall #-}

module Casadi.Internal.CppHelpers
       ( newStdVec
       , readStdVec
       , c_newStdString
       , c_deleteStdString
       , c_lengthStdString
       , c_copyStdString
       ) where

import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import Foreign.Marshal ( mallocArray, free, peekArray, withArrayLen )

import Casadi.Internal.MarshalTypes
import Casadi.Internal.CToolsImports

newStdVec :: V.Vector (Ptr a) -> IO (Ptr (StdVec (Ptr a)))
newStdVec vec = do
  withArrayLen (V.toList vec) $ \num array ->
    c_newVecVoidP array (fromIntegral num)

readStdVec :: Ptr (StdVec (Ptr a)) -> IO (V.Vector (Ptr a))
readStdVec vecPtr = do
  n <- fmap fromIntegral (c_sizeVecVoidP vecPtr)
  arr <- mallocArray n
  c_copyVecVoidP vecPtr arr
  ret <- peekArray n arr
  free arr
  return (V.fromList ret)

foreign import ccall unsafe "hs_new_string" c_newStdString
  :: Ptr CChar -> IO (Ptr StdString)
foreign import ccall unsafe "hs_delete_string" c_deleteStdString
  :: Ptr StdString -> IO ()
foreign import ccall unsafe "hs_length_string" c_lengthStdString
  :: Ptr StdString -> IO CInt
foreign import ccall unsafe "hs_copy_string" c_copyStdString
  :: Ptr StdString -> Ptr CChar -> IO ()
