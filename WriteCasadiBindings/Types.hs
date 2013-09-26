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
                                 , ThreeVectors
                                 ) where

import WriteCasadiBindings.CasadiClasses ( CasadiClass(..) )

data Method = Method { fName :: Name
                     , fType :: Type
                     , fArgs :: [Type]
                     , fMethodType :: MethodType
                     } deriving Show
data MethodType = Constructor | Static | Normal deriving Show

data Class = Class CasadiClass [Method] deriving Show
data Function = Function Name Type [Type] deriving Show

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
               deriving Show

data Primitive' a = NonVec Primitive
                  | Vec a
                  deriving Show

type ThreeVectors = (Primitive' (Primitive' (Primitive' (Primitive' ()))))

data Type = Val ThreeVectors
          | Ref ThreeVectors
          | ConstRef ThreeVectors
          deriving Show
