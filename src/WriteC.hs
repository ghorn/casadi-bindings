{-# OPTIONS_GHC -Wall #-}
{-# Language OverloadedStrings #-}

module WriteC ( writeFunction
              , writeClass
              , writeMethod
              , deleteName
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
  , unlines marshalls
  , writeReturn retType call
  , "}"
  , ""
  ]
  where
    marshalls = map (uncurry marshall) $ zip [0..] params
    proto = cppRetType ++ " " ++ cName ++ protoArgs
    cppRetType = fromRetType retType
    cName = toCName cppName
    cppName = functionName
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    allProtoArgs = map (uncurry paramProto) $ zip [0..] params
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] params) ++ ")"
    call = cppName ++ args

writeClass :: Class -> [String]
writeClass c@(Class classType methods) =
  writeClassDelete c : map (writeMethod classType) methods

deleteName :: CasadiPrimitive -> String
deleteName classType = "delete_" ++ cType (Prim (CP classType))

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
    cName = deleteName classType
    classname = cppType (Ptr (CP classType))
    protoArgs = "(" ++ classname ++ " obj)"

paramName :: Int -> String
paramName k = "x" ++ show k

fromRetType :: RetType -> String
fromRetType (SimpleType x) = cppType x
fromRetType (NewRef x) = cppType (Ptr (CP x))

paramProto :: Int -> Type -> String
paramProto k t = cWrapperType t ++ " " ++ paramName k

marshall :: Int -> Type -> String
marshall k t = "    " ++ cppMarshallType t ++ " " ++ paramName k ++
               "_ = marshall(" ++
               paramName k ++ ");"

writeMethod :: CasadiPrimitive -> Method -> String
writeMethod classType fcn =
  unlines
  [ "// ================== " ++ static ++ "method: " ++ show methodName ++ " ==============="
  , "// class: " ++ show (cppType (Prim (CP classType)))
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
  , unlines marshalls
  , writeReturn (fType fcn) call
  , "}"
  , ""
  ]
  where
    marshalls = map (uncurry marshall) $ zip [0..] (fArgs fcn)
    proto = cppRetType ++ " " ++ cName ++ protoArgs
    cppRetType = fromRetType (fType fcn)
    cName = toCName cppName
    static = if fStatic fcn == Static True then "static " else ""
    cppName = cppType (Prim (CP classType)) ++ "::" ++ methodName
    Name methodName = fName fcn
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (fArgs fcn)
    allProtoArgs
      | fStatic fcn == Static True = nonSelfProtoArgs
      | otherwise = (cppType (Ptr (CP classType)) ++ " obj") : nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] (fArgs fcn)) ++ ")"
    call
      | fStatic fcn == Static True = cppName ++ args
      | otherwise = "obj->" ++ methodName ++ args
    
writeReturn :: RetType -> String -> String
writeReturn (SimpleType _) x = "    return " ++ x ++ ";"
writeReturn (NewRef retType) x = "    return new " ++ cppType (Prim (CP retType)) ++ "( " ++ x ++ " );"
