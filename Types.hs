{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module Types ( Class(..)
             , Const(..)
             , Function(..)
             , Method(..)
             , Name(..)
             , Type(..)
             , RetType(..)
             , Static(..)
             ) where

data Name = Name String deriving Show
data RetType = SimpleType Type
             | NewRef Type deriving Show

data Type = Type String
          | Ptr Type
          | Ref Type
          deriving Show

data Method = Method { fName :: Name
                     , fType :: RetType
                     , fArgs :: [Type]
                     , fConst :: Const
                     , fStatic :: Static
                     } deriving Show
--data Variable = Variable Name Type
newtype Const = Const Bool deriving (Show, Eq)
newtype Static = Static Bool deriving (Show, Eq)

data Class = Class Name [Method] deriving Show --[Variable]
data Function = Function Name RetType [Type] deriving Show

