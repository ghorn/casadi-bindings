{-# OPTIONS_GHC -Wall #-}

module WriteBindings.Types
       ( Class(..)
       , CppFunction(..)
       , Method(..)
       , Name(..)
       , Type(..)
       , MethodType(..)
       , CasadiClass(..)
       , CasadiEnum(..)
       , Doc(..)
       , CEnum(..)
       , EnumType(..)
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
data CppFunction = CppFunction Name Type [Type] Doc deriving Show

data Name = Name String deriving Show

data Type = CInt
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
          | Ref Type
          | ConstRef Type
          | StdVec Type
          deriving (Show, Eq)

data EnumType = EnumInt | EnumUInt deriving (Show, Eq)
data CEnum = CEnum CasadiEnum Doc EnumType [(String,Doc,Integer)] deriving (Show, Eq)
