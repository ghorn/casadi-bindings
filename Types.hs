{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module Types ( Class(..)
             , Const(..)
             , Function(..)
             , Method(..)
             , Name(..)
             , Param(..)
             , RetType(..)
             , Static(..)
             ) where

data Name = Name String deriving Show
data RetType = SimpleType String
             | NewRef String deriving Show

data Method = Method { fName :: Name
                     , fType :: RetType
                     , fArgs :: [Param]
                     , fConst :: Const
                     , fStatic :: Static
                     } deriving Show
--data Variable = Variable Name Type
newtype Const = Const Bool deriving (Show, Eq)
newtype Static = Static Bool deriving (Show, Eq)

data Param = Param String deriving Show
data Class = Class Name [Method] deriving Show --[Variable]
data Function = Function Name RetType [Param] deriving Show

