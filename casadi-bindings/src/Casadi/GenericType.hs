{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-cse #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

-- todo(greg): merge this with Casadi.GenericC
module Casadi.GenericType
       ( GenericType
       , TypeID(..)
       , GType(..)
       , fromGType, toGType, toGType'
       , getDescription
       , getType
       ) where

import qualified Data.Traversable as T
import Data.Vector ( Vector )
import Data.Map ( Map )
import qualified Data.Map as M

import Casadi.Core.Classes.Function ( Function )
import Casadi.Core.Classes.GenericType
import Casadi.Core.Enums ( TypeID(..) )

import Casadi.SharedObject ( castSharedObject )

instance Show Function where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

instance Show GenericType where
  show x = show (castSharedObject x)
  {-# NOINLINE show #-}

getDescription :: GenericType -> IO String
getDescription = genericType_get_description

getType :: GenericType -> IO TypeID
getType = genericType_getType


-- | Haskell version of Casadi.GenericType
data GType
  = GBool Bool
  | GDouble Double
  | GInt Int
  | GString String
  | GBoolVec (Vector Bool)
  | GDoubleVec (Vector Double)
  | GIntVec (Vector Int)
  | GIntVecVec (Vector (Vector Int))
  | GStringVec (Vector String)
--  | GGenericType GenericType
  | GFunction Function
  | GDict (Map String GType)
  deriving Show

instance Eq GType where
  (==) (GFunction _) _ = error "can't compare GFunctions"
  (==) _ (GFunction _) = error "can't compare GFunctions"
  (==) (GBool x) (GBool y) = x == y
  (==) (GBool _) _ = False
  (==) (GDouble x) (GDouble y)
    = x == y || all isNaN [x, y]
  (==) (GDouble _) _ = False
  (==) (GInt x) (GInt y) = x == y
  (==) (GInt _) _ = False
  (==) (GString x) (GString y) = x == y
  (==) (GString _) _ = False
  (==) (GBoolVec x) (GBoolVec y) = x == y
  (==) (GBoolVec _) _ = False
  (==) (GDoubleVec x) (GDoubleVec y) = x == y
  (==) (GDoubleVec _) _ = False
  (==) (GIntVec x) (GIntVec y) = x == y
  (==) (GIntVec _) _ = False
  (==) (GIntVecVec x) (GIntVecVec y) = x == y
  (==) (GIntVecVec _) _ = False
  (==) (GStringVec x) (GStringVec y) = x == y
  (==) (GStringVec _) _ = False
  (==) (GDict x) (GDict y) = f (M.toList x) (M.toList y)
    where
      f ((nx,gx):xs) ((ny,gy):ys)
        | nx /= ny = False
        | not ((==) gx gy) = False
        | otherwise = f xs ys
      f [] [] = True
      f _ _ = False
  (==) (GDict _) _ = False

fromGType :: GType -> IO GenericType
fromGType (GBool r) = genericType__10 r
fromGType (GDouble r) = genericType__8 r
fromGType (GInt r) = genericType__9 r
fromGType (GString r) = genericType__7 r
fromGType (GBoolVec r) = do
  gt <- genericType__6 r
  gtype <- getType gt
  case gtype of
    OT_BOOLVECTOR -> error "fromGType GBoolVec got OT_BOOLVECTOR, which means a casadi issue was fixed and this special casing code should be removed"
    OT_INTVECTOR -> return gt
    other -> error $ "fromGType GBoolVec got " ++ show other ++ ", which is really weird"
fromGType (GDoubleVec r) = genericType__3 r
fromGType (GIntVec r) = genericType__5 r
fromGType (GIntVecVec r) = genericType__4 r
fromGType (GStringVec r) = genericType__2 r
fromGType (GFunction r) = genericType__1 r
fromGType (GDict r) = T.mapM fromGType r >>= genericType__0

toGType :: GenericType -> IO GType
toGType gt = do
  ego <- toGType' gt
  case ego of
    Left err -> error err
    Right r -> return r

toGType' :: GenericType -> IO (Either String GType)
toGType' gt = do
  typeID <- getType gt
  description <- getDescription gt

  let unhandledTypeMsg = "unhandled GenericType " ++ show description ++ " (" ++ show typeID ++ ")"

  case typeID of
    OT_BOOL -> Right . GBool <$> genericType_to_bool gt
    OT_BOOLVECTOR -> Right . GBoolVec <$> genericType_to_bool_vector gt
    OT_DICT -> do
      dict0 <- genericType_to_dict gt :: IO (M.Map String GenericType)
      dict1 <- T.mapM toGType' dict0 :: IO (M.Map String (Either String GType))
      return (fmap GDict (sequenceA dict1))
    OT_DOUBLE -> Right . GDouble <$> genericType_to_double gt
    OT_DOUBLEVECTOR -> Right . GDoubleVec <$> genericType_to_double_vector gt
    OT_FUNCTION -> Right . GFunction <$> genericType_to_function gt
    OT_INT -> Right . GInt <$> genericType_to_int gt
    OT_INTVECTOR -> Right . GIntVec <$> genericType_to_int_vector gt
    OT_INTVECTORVECTOR -> Right . GIntVecVec <$> genericType_to_int_vector_vector gt
    OT_STRING -> Right . GString <$> genericType_to_string gt
    OT_STRINGVECTOR -> Right . GStringVec <$> genericType_to_string_vector gt
    OT_NULL -> return $ Left unhandledTypeMsg
    OT_UNKNOWN -> return $ Left unhandledTypeMsg
    OT_VOIDPTR -> return $ Left unhandledTypeMsg
