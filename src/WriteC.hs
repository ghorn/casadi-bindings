{-# OPTIONS_GHC -Wall #-}

module WriteC ( writeFunction
              , writeClass
              ) where

import Data.List ( intercalate )

import Types

paramName :: Int -> String
paramName k = "x" ++ show k

paramProto :: Int -> Type -> String
paramProto k t = cWrapperType t ++ " " ++ paramName k

marshall :: Int -> Type -> String
marshall k t = "    " ++ cppMarshallType t ++ " " ++ paramName k ++
               "_ = marshall(" ++
               paramName k ++ ");"


writeFunction :: Function -> String
writeFunction (Function (Name functionName) retType params) =
  unlines
  [ "// ================== function " ++ show functionName ++ " ==============="
  , "// cppName: " ++ show cppName
  , "// cName: " ++ show cName
  , "// protoArgs: " ++ show protoArgs
  , "// params: " ++ show params
  , "// args: " ++ show args
  , "// cWrapperRetType: " ++ show (cWrapperRetType retType)
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
    proto = cWrapperRetType retType ++ " " ++ cName ++ protoArgs
    cName = toCName cppName
    cppName = functionName
    protoArgs = "(" ++ intercalate ", " protoArgList ++ ")"
    protoArgList = map (uncurry paramProto) $ zip [0..] params
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] params) ++ ")"
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
    cName = deleteName classType
    classname = cppClassName classType
    protoArgs = "(" ++ classname ++ "* obj)"

writeMethod :: CasadiClass -> Method -> String
writeMethod classType fcn =
  unlines
  [ "// ================== " ++ show (fMethodType fcn) ++ " method: " ++ show methodName ++ " ==============="
  , "// class: " ++ show (cppClassName classType)
  , "// cppName: " ++ show cppName
  , "// cName: " ++ show cName
  , "// protoArgs: " ++ show protoArgs
  , "// args: " ++ show args
  , "// rettype: " ++ show retType
  , "// cWrapperRetType: " ++ show (cWrapperRetType retType)
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
    retType = fType fcn
    marshalls = map (uncurry marshall) $ zip [0..] (fArgs fcn)
    proto = cWrapperRetType retType ++ " " ++ cName ++ protoArgs
    cName = toCName cppName
    cppName = cppMethodName classType fcn
    Name methodName = fName fcn
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (fArgs fcn)
    allProtoArgs = case fMethodType fcn of
      Normal -> (cppClassName classType ++ "* obj") : nonSelfProtoArgs
      _ -> nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] (fArgs fcn)) ++ ")"
    call = case fMethodType fcn of
      Normal -> "obj->" ++ methodName ++ args
      _ -> cppName ++ args
