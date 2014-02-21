{-# OPTIONS_GHC -Wall #-}

module WriteBindings.Types
       ( Class(..)
       , Function(..)
       , Method(..)
       , Name(..)
       , Type(..)
       , MethodType(..)
       , Primitive(..)
       , Primitive'(..)
       , CasadiClass(..)
       , CasadiEnum(..)
       , Doc(..)
       , CEnum(..)
       , EnumType(..)
       , ThreeVectors
       ) where

import WriteBindings.Buildbot.CasadiClasses ( CasadiClass(..), CasadiEnum(..) )

data Method = Method { fName :: Name
                     , fType :: Type
                     , fArgs :: [Type]
                     , fMethodType :: MethodType
                     , fDoc :: Doc
                     } deriving Show
data MethodType = Constructor | Static | Normal deriving Show

newtype Doc = Doc String deriving (Show, Eq)
data Class = Class CasadiClass [Method] Doc deriving Show
data Function = Function Name Type [Type] Doc deriving Show

data Name = Name String deriving Show

data Primitive = CInt
               | CDouble
               | CBool
               | CVoid
               | CSize
               | CLong
               | CUChar
               | StdOstream
               | StdString
               | CasadiClass CasadiClass
               | CasadiEnum CasadiEnum
               deriving (Show, Eq)

data Primitive' a = NonVec Primitive
                  | Vec a
                  deriving Show

type ThreeVectors = (Primitive' (Primitive' (Primitive' (Primitive' ()))))

data Type = Val ThreeVectors
          | Ref ThreeVectors
          | ConstRef ThreeVectors
          deriving Show

data EnumType = EnumInt | EnumUInt deriving (Show, Eq)
data CEnum = CEnum CasadiEnum Doc EnumType [(String,Doc,Integer)] deriving (Show, Eq)
