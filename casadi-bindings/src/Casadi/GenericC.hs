{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

module Casadi.GenericC
       ( GenericC(..)
       , Opt(..)
       , getDescription
       ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Foreign.Marshal ( new, free )
import Foreign.Storable ( peek )
import Foreign.Ptr ( Ptr, nullPtr )

import Casadi.Internal.FormatException ( formatException )
import Casadi.Internal.MarshalTypes ( StdVec, StdString )
import Casadi.Internal.Marshal ( withMarshal )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )

import Casadi.Core.Data ( GenericType' )
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
  mkGeneric = genericType__1
  fromGeneric = const $ return $ error "no fromGeneric for DerivativeGenerator"
instance GenericC [(String,Opt)] where
  mkGeneric kvs = do
    let (ks,vs') = unzip kvs
    vs <- mapM (\(Opt x) -> mkGeneric x) vs'
    mkGenericDictionary (V.fromList ks) (V.fromList vs)
  fromGeneric = const $ return $ error "no fromGeneric for [(String,Opt)]"

ifThenGet :: (a -> IO Bool) -> (a -> IO b) -> a -> IO (Maybe b)
ifThenGet isOpt getOpt g = do
  isopt <- isOpt g
  if isopt
    then fmap Just (getOpt g)
    else return Nothing

-- direct wrapper
foreign import ccall unsafe "custom_generic_dictionary" c_custom_generic_dictionary
  :: Ptr (Ptr StdString) -> Ptr (StdVec (Ptr StdString)) -> Ptr (StdVec (Ptr GenericType'))
     -> IO (Ptr GenericType')

mkGenericDictionary :: Vector String -> Vector GenericType -> IO GenericType
mkGenericDictionary x0 x1 =
  withMarshal x0 $ \x0' ->
  withMarshal x1 $ \x1' ->
  do
    errStrPtrP <- new nullPtr
    ret <- c_custom_generic_dictionary errStrPtrP x0' x1'
    errStrPtr <- peek errStrPtrP
    free errStrPtrP
    if errStrPtr == nullPtr then wrapReturn ret else wrapReturn errStrPtr >>= (error . formatException)
