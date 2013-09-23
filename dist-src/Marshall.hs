{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

module Marshall ( withMarshall
                ) where

import Data.Char ( toLower )
import Data.List ( intersperse )
import Types
import CasadiTree
import qualified Foreign.C.Types as C
import qualified Foreign.Ptr as F

toLower' :: String -> String
toLower' [] = []
toLower' (x:xs) = toLower x:xs

data StdStr = StdStr
marshallStr :: String -> IO (F.Ptr StdStr)
marshallStr = undefined

freeStr :: F.Ptr StdStr -> IO ()
freeStr = undefined

c_boo :: F.Ptr StdStr -> F.Ptr StdStr -> IO C.CInt
c_boo = undefined

boo :: String -> String -> IO C.CInt
boo x0' x1' = do
  withMarshall x0' $ \x0 ->
    withMarshall x1' $ \x1 ->
    c_boo x0 x1

withMarshall :: Marshall a b => a -> (b -> IO c) -> IO c
withMarshall x' f = do
  x <- marshall x'
  ret <- f x
  unmarshall x
  return ret

class Marshall a b | b -> a, a -> b where
  marshall :: a -> IO b
  unmarshall :: b -> IO ()
  unmarshall _ = return ()

instance Marshall Int C.CInt where
  marshall x = return (fromIntegral x)
instance Marshall String (F.Ptr StdStr) where
  marshall x = marshallStr x
  unmarshall = freeStr
