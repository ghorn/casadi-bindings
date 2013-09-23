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
             , hsType
             , hsType'
             , ffiType
             , ffiType'
             , cppType
             , cType
             , toCName
             ) where

import qualified Data.Text as T

data Name = Name String deriving Show
data RetType = SimpleType Type
             | NewRef Type deriving Show

data Type = CInt
          | SXMatrixVector
          | StdString
          | FX
          | MX
          | SXMatrix
          | SXFunction
          | Ref Type
          | Ptr Type
          deriving Show

casadiNs :: String -> String
casadiNs = ("CasADi::" ++)

hsType :: Type -> String
hsType = hsType' False

hsType' :: Bool -> Type -> String
hsType' _ CInt = "Int"
hsType' _ StdString = "String"
hsType' _ FX = "FX"
hsType' _ SXFunction = "SXFunction"
hsType' _ MX = "MX"
hsType' _ SXMatrix = "SXMatrix"
hsType' _ SXMatrixVector = "SXMatrixVector"
hsType' p (Ref x) = hsType' p x
hsType' p (Ptr x) = hsType' p x

maybeParens :: Bool -> String -> String
maybeParens False x = x
maybeParens True x = "(" ++ x ++ ")"

ffiType :: Type -> String
ffiType = ffiType' False

ffiType' :: Bool -> Type -> String
ffiType' _ CInt = "CInt"
ffiType' _ StdString = "StdString"
ffiType' _ FX = "FX"
ffiType' _ SXFunction = "SXFunction"
ffiType' _ MX = "MX"
ffiType' _ SXMatrix = "SXMatrix"
ffiType' _ SXMatrixVector = "SXMatrixVector"
ffiType' p (Ref x) = maybeParens p $ "Ptr " ++ ffiType' True x
ffiType' p (Ptr x) = maybeParens p $ "Ptr " ++ ffiType' True x

cppType :: Type -> String
cppType SXMatrixVector = "std::vector<CasADi::SXMatrix>"
cppType CInt = "int"
cppType StdString = "std::string"
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

