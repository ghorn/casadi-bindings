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
             , cppType
             , cType
             , toCName
             ) where

import qualified Data.Text as T

data Name = Name String deriving Show
data RetType = SimpleType Type
             | NewRef Type deriving Show

data Type = CInt
          | VectorSXMat
          | StdStr
          | FX
          | MX
          | SXMatrix
          | SXFunction
          | Ref Type
          | Ptr Type
          deriving Show

casadiNs :: String -> String
casadiNs = ("CasADi::" ++)

cppType :: Type -> String
cppType VectorSXMat = "std::vector<CasADi::SXMatrix>"
cppType CInt = "int"
cppType StdStr = "std::string"
cppType FX = casadiNs "FX"
cppType SXFunction = casadiNs "SXFunction"
cppType MX = casadiNs "MX"
cppType SXMatrix = casadiNs "SXMatrix"
cppType (Ref x) = cppType x ++ "&"
cppType (Ptr x) = cppType x ++ "*"

cType :: Type -> String
cType = toCName . cppType

toCName :: String -> String
toCName cppName = T.unpack (replaces [(":","_"),(" >","_"),("< ","_")] (T.pack cppName))
  where
    replaces :: [(T.Text,T.Text)] -> T.Text -> T.Text
    replaces ((find',replace'):xs) = replaces xs . T.replace find' replace'
    replaces [] = id

data Method = Method { fName :: Name
                     , fType :: RetType
                     , fArgs :: [Type]
                     , fConst :: Const
                     , fStatic :: Static
                     } deriving Show
--data Variable = Variable Name Type
newtype Const = Const Bool deriving (Show, Eq)
newtype Static = Static Bool deriving (Show, Eq)

data Class = Class Type [Method] deriving Show --[Variable]
data Function = Function Name RetType [Type] deriving Show

