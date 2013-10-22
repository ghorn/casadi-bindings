{-# OPTIONS_GHC -Wall #-}

module WriteCasadiBindings.Types ( Class(..)
                                 , Function(..)
                                 , Method(..)
                                 , Name(..)
                                 , Type(..)
                                 , MethodType(..)
                                 , Primitive(..)
                                 , Primitive'(..)
                                 , CasadiClass(..)
                                 , Doc(..)
                                 , CEnum(..)
                                 , EnumType(..)
                                 , ThreeVectors
                                 ) where

import WriteCasadiBindings.Buildbot.CasadiClasses ( CasadiClass(..) )

data Method = Method { fName :: Name
                     , fType :: Type
                     , fArgs :: [Type]
                     , fMethodType :: MethodType
                     , fDoc :: Doc
                     } deriving Show
data MethodType = Constructor | Static | Normal deriving Show

newtype Doc = Doc String deriving Show
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
               deriving (Show, Eq)

data Primitive' a = NonVec Primitive
                  | Vec a
                  deriving Show

type ThreeVectors = (Primitive' (Primitive' (Primitive' (Primitive' ()))))

data Type = Val ThreeVectors
          | Ref ThreeVectors
          | ConstRef ThreeVectors
          deriving Show

data EnumType = EnumInt | EnumUInt
data CEnum = CEnum String Doc EnumType [(String,Doc,Integer)]
