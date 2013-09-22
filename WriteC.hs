{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module WriteC ( writeFunction
              , writeClass
              , writeMethod
              ) where

import Data.List ( intercalate )
import qualified Data.Text as T

import Types

writeFunction :: Function -> String
writeFunction (Function (Name cppName) retType params) = 
  unlines
  [ "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , writeReturn retType $ cppName ++
    "(" ++ intercalate ", " (map (paramName . fst) args) ++ ")"
  , "}"
  ]
  where
    args = zip [0..] params
    proto = cname ++ "(" ++ intercalate ", " (map (uncurry paramProto) args) ++ ")"
    shownType = showType retType
    cname = shownType ++ " " ++
            T.unpack (replaces [(":","_"),(" >","_"),("< ","_")] (T.pack cppName))

replaces :: [(T.Text,T.Text)] -> T.Text -> T.Text
replaces ((find',replace'):xs) = replaces xs . T.replace find' replace'
replaces [] = id

writeClass :: Class -> [String]
writeClass (Class name methods) = map (writeMethod name) methods

paramProto :: Int -> Param -> String
paramProto k (Param t) = t ++ " " ++ paramName k

paramName :: Int -> String
paramName k = "x" ++ show k

showType :: RetType -> String
showType (SimpleType x) = x
showType (NewRef x) = x ++ "*"

writeMethod :: Name -> Method -> String
writeMethod (Name classname) fcn
  | fStatic fcn == Static True =
    staticMethod cname classname name args (fType fcn)
  | otherwise = nonStaticMethod cname classname name args (fType fcn)
  where
    cppName = classname ++ "::" ++ name
    maybeConst = if fConst fcn == Const True then "const__" else ""
    shownType = showType (fType fcn)
    cname = shownType ++ " " ++ maybeConst ++
            T.unpack (replaces [(":","_"),(" >","_"),("< ","_")] (T.pack cppName))
    args = zip [0..] (fArgs fcn)
    Name name = fName fcn

writeReturn :: RetType -> String -> String
writeReturn (SimpleType _) x = "    return " ++ x ++ ";"
writeReturn (NewRef retType) x = "    return new " ++ retType ++ "( " ++ x ++ " );"

staticMethod :: String -> String -> String -> [(Int,Param)] -> RetType -> String
staticMethod cname classname name args retType =
  unlines
  [ "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , writeReturn retType $ classname ++ "::" ++ name ++
    "(" ++ intercalate ", " (map (paramName . fst) args) ++ ")"
  , "}"
  ]
  where
    proto = cname ++ "(" ++ intercalate ", " (map (uncurry paramProto) args) ++ ")"

nonStaticMethod :: String -> String -> String -> [(Int,Param)] -> RetType -> String
nonStaticMethod cdef classname name args retType =
  unlines
  [ "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , writeReturn retType $ "obj->" ++ name ++
    "(" ++ intercalate ", " (map (paramName . fst) args) ++ ")"
  , "}"
  ]
  where
    proto = cdef ++ "(" ++ intercalate ", " (selfname : argnames) ++ ")"
    selfname = classname ++ "* obj" :: String
    argnames = (map (uncurry paramProto) args) :: [String]

