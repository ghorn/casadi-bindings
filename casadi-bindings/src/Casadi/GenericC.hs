{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

-- todo(greg): merge this with Casadi.Option
module Casadi.GenericC
       ( GenericC(..)
       , GenericType
       , Opt(..)
       , getDescription
       ) where

import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Data.Map ( Map )
import System.IO.Unsafe ( unsafePerformIO )

import Casadi.Core.Classes.Function ( Function )
import Casadi.Core.Classes.GenericType
import Casadi.Core.Classes.DerivativeGenerator ( DerivativeGenerator )

import Casadi.SharedObject ( castSharedObject )

instance Show GenericType where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

class GenericC a where
  mkGeneric :: a -> IO GenericType
  fromGeneric :: GenericType -> IO (Maybe a)

data Opt where
  Opt :: GenericC a => a -> Opt
instance Show Opt where
  show (Opt x) = show (unsafePerformIO (mkGeneric x))

getDescription :: GenericType -> IO String
getDescription = genericType_get_description

instance GenericC Bool where
  mkGeneric = genericType__12
  fromGeneric = ifThenGet genericType_isBool genericType_toBool
instance GenericC Int where
  mkGeneric = genericType__11
  fromGeneric = ifThenGet genericType_isInt genericType_toInt
instance GenericC Double where
  mkGeneric = genericType__10
  fromGeneric = ifThenGet genericType_isDouble genericType_toDouble
instance GenericC String where
  mkGeneric = genericType__9
  fromGeneric = ifThenGet genericType_isString genericType_toString
instance GenericC (Vector Bool) where
  mkGeneric = genericType__8
  fromGeneric = const (return Nothing)
instance GenericC (Vector Int) where
  mkGeneric = genericType__7
  fromGeneric = ifThenGet genericType_isIntVector genericType_toIntVector
instance GenericC (Vector Double) where
  mkGeneric = genericType__5
  fromGeneric = ifThenGet genericType_isDoubleVector genericType_toDoubleVector
instance GenericC (Vector String) where
  mkGeneric = genericType__4
  fromGeneric = ifThenGet genericType_isStringVector genericType_toStringVector
instance GenericC GenericType where
  mkGeneric = return
  fromGeneric = return . Just
instance GenericC Function where
  mkGeneric = genericType__3
  fromGeneric = ifThenGet genericType_isFunction genericType_toFunction
instance GenericC DerivativeGenerator where
  mkGeneric = genericType__2
  fromGeneric = const $ return $ error "no fromGeneric for DerivativeGenerator"
instance GenericC Opt where
  mkGeneric (Opt o) = mkGeneric o
  fromGeneric = const $ return $ error "no fromGeneric for Opt"
instance GenericC a => GenericC (Map String a) where
  mkGeneric dict = T.mapM mkGeneric dict >>= genericType__0
  fromGeneric gdict = do
    mDict <- ifThenGet genericType_isDict genericType_toDict gdict
    case mDict of
     Nothing -> return Nothing
     Just dict -> do
       T.sequenceA <$> T.mapM fromGeneric dict

ifThenGet :: (a -> IO Bool) -> (a -> IO b) -> a -> IO (Maybe b)
ifThenGet isOpt getOpt g = do
  isopt <- isOpt g
  if isopt
    then fmap Just (getOpt g)
    else return Nothing
