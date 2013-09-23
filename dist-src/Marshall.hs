{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

module Marshall ( withMarshall
                , StdString
                ) where

import qualified Foreign.C.Types as C
import qualified Foreign.Ptr as F

data StdString = StdString

withMarshall :: Marshall a b => a -> (b -> IO c) -> IO c
withMarshall x' f = do
  x <- marshall x'
  ret <- f x
  unmarshall x
  return ret

class Marshall a b | b -> a, a -> b where
  marshall :: a -> IO b
  unmarshall :: b -> IO ()

instance Marshall Int C.CInt where
  marshall x = return (fromIntegral x)
instance Marshall String (F.Ptr StdString) where
  marshall x = marshallStr x
  unmarshall = freeStr

instance Marshall SXFunction SXFunction where
  marshall = id
  unmarshall = return ()
