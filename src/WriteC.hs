{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module WriteC ( writeFunction
              , writeClass
              , writeMethod
              ) where

import Data.List ( intercalate )

import Types

writeFunction :: Function -> String
writeFunction (Function (Name functionName) retType params) =
  unlines
  [ "// ================== function " ++ show functionName ++ " ==============="
  , "// cppName: " ++ show cppName
  , "// cName: " ++ show cName
  , "// protoArgs: " ++ show protoArgs
  , "// args: " ++ show args
  , "// rettype: " ++ show retType
  , "// rettype-cpp: " ++ show cppRetType
  , "// proto: " ++ show proto
  , "// call: " ++ show call
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , writeReturn retType call
  , "}"
  , ""
  ]
  where
    proto = cppRetType ++ " " ++ cName ++ protoArgs
    cppRetType = cppType $ fromRetType retType
    cName = toCName cppName
    cppName = functionName
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] params
    allProtoArgs = nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map (paramName . fst) $ zip [0..] params) ++ ")"
    call = cppName ++ args

writeClass :: Class -> [String]
writeClass c@(Class classType methods) =
  writeClassDelete c : map (writeMethod classType) methods

writeClassDelete :: Class -> String
writeClassDelete (Class classType _) =
  unlines
  [ "// ================== delete "++ show classname ++"==============="
  , "// classname: " ++ show classname
  , "// cName: " ++ show cName
  , "// protoArgs: " ++ show protoArgs
  , "// proto: " ++ show proto
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , "    delete obj;"
  , "}"
  , ""
  ]
  where
    proto = "void " ++ cName ++ protoArgs
    cName = "delete_" ++ toCName classname
    classname = cppType classType
    protoArgs = "(" ++ cppType (Ptr classType) ++ " obj)"

paramName :: Int -> String
paramName k = "x" ++ show k

fromRetType :: RetType -> Type
fromRetType (SimpleType x) = x
fromRetType (NewRef x) = Ptr x

paramProto :: Int -> Type -> String
paramProto k t = cppType t ++ " " ++ paramName k

writeMethod :: Type -> Method -> String
writeMethod class' fcn =
  unlines
  [ "// ================== " ++ static ++ "method: " ++ show methodName ++ " ==============="
  , "// class: " ++ show classname
  , "// cppName: " ++ show cppName
  , "// cName: " ++ show cName
  , "// protoArgs: " ++ show protoArgs
  , "// args: " ++ show args
  , "// rettype: " ++ show (fType fcn)
  , "// rettype-cpp: " ++ show cppRetType
  , "// proto: " ++ show proto
  , "// call: " ++ show call
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , writeReturn (fType fcn) call
  , "}"
  , ""
  ]
  where
    proto = cppRetType ++ " " ++ cName ++ protoArgs
    cppRetType = cppType $ fromRetType (fType fcn)
    cName = toCName cppName
    static = if fStatic fcn == Static True then "static " else ""
    cppName = classname ++ "::" ++ methodName
    Name methodName = fName fcn
    classname = cppType class'
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (fArgs fcn)
    allProtoArgs
      | fStatic fcn == Static True = nonSelfProtoArgs
      | otherwise = (cppType (Ptr class') ++ " obj") : nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map (paramName . fst) $ zip [0..] (fArgs fcn)) ++ ")"
    call
      | fStatic fcn == Static True = cppName ++ args
      | otherwise = "obj->" ++ methodName ++ args
    
writeReturn :: RetType -> String -> String
writeReturn (SimpleType _) x = "    return " ++ x ++ ";"
writeReturn (NewRef retType) x = "    return new " ++ cppType retType ++ "( " ++ x ++ " );"
