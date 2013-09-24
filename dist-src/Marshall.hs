{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

module Marshall ( Marshall(..)
                , StdString
                ) where

import qualified Foreign.C.Types as C
import qualified Foreign.C.String as C
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )

data StdString

class Marshall a b where
  withMarshall :: a -> (b -> IO c) -> IO c

instance Marshall Int C.CInt where
  withMarshall x f = f (fromIntegral x)

foreign import ccall unsafe "marshall_stdstr" c_marshallStdString
  :: C.CString -> IO (Ptr StdString)
foreign import ccall unsafe "free_stdstr" c_freeStdString
  :: Ptr StdString -> IO ()

instance Marshall String (Ptr StdString) where
  withMarshall x f = do
    C.withCString x $ \cstr -> do
      stdStr <- c_marshallStdString cstr
      ret <- f stdStr
      c_freeStdString stdStr
      return ret
instance Marshall (ForeignPtr a) (Ptr a) where
    withMarshall x f = withForeignPtr x f
