{-# OPTIONS_GHC -Wall #-}

module WriteBindings.WriteC
       ( writeFunctions
       , writeClass
       ) where

import Data.List ( intercalate )

import WriteBindings.ParseJSON
import qualified WriteBindings.TypeMaps as TM

paramName :: Int -> String
paramName k = "x" ++ show k

paramProto :: Int -> Type -> String
paramProto k t = TM.cWrapperType t ++ " " ++ paramName k

-- todo: preserve constref if cWrapperType and cppMarshalType are the same
marshal :: Int -> Type -> String
marshal k t = "    " ++ TM.cppMarshalType t ++ " " ++ paramName k ++
              "_ = Marshaling<" ++ TM.cppMarshalType t ++ "," ++ TM.cWrapperType t ++ ">::marshal(" ++
              paramName k ++ ");"

writeFunctions :: CppFunctions -> [String]
writeFunctions (Left fs) = map writeFunction fs
writeFunctions (Right f) = [writeFunction f]

writeFunction :: CppFunction -> String
writeFunction fcn =
  unlines
  [ "// ================== function " ++ show cppName ++ " ==============="
  , "// cppName: " ++ show cppName
  , "// cWrapperName: " ++ show cWrapperName''
  , "// protoArgs: " ++ show protoArgs
  , "// params: " ++ show params
  , "// retType: " ++ show retType
  , "// args: " ++ show args
  , "// cWrapperRetType: " ++ show (TM.cWrapperRetType retType)
  , "// proto: " ++ show proto
  , "// call: " ++ show call
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , unlines marshals
  , TM.writeReturn retType call
  , "}"
  , ""
  ]
  where
    cppName = "casadi::" ++ fName fcn
    retType = fReturn fcn
    params = fParams fcn
    marshals = map (uncurry marshal) $ zip [0..] params
    proto = TM.cWrapperRetType retType ++ " " ++ cWrapperName'' ++ protoArgs
    cWrapperName'' = TM.cWrapperName' fcn
    protoArgs = "(" ++ intercalate ", " protoArgList ++ ")"
    protoArgList = map (uncurry paramProto) $ zip [0..] params
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] params) ++ ")"
    call = removeTics cppName ++ args

writeClass :: Class -> String
writeClass c = unlines $ writeDeletes ct : concatMap (writeMethods ct) (clMethods c)
  where
    ClassType ct = clType c

writeDeletes :: Type -> String
writeDeletes ut =
  unlines
  [ "// ================== delete "++ show ut ++"==============="
  , "// classType: " ++ show ut
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , "    delete obj;"
  , "}"
  ]
  where
    proto = "void " ++ TM.deleteName ut ++ "(" ++ TM.cWrapperType ut ++ " obj)"

removeTics :: String -> String
removeTics = reverse . removeTics' . reverse
  where
    removeTics' ('\'':xs) = removeTics' xs
    removeTics' x = x

writeMethods :: Type -> Methods -> [String]
writeMethods t (Left fs) = map (writeMethod t) fs
writeMethods t (Right f) = [writeMethod t f]

writeMethod :: Type -> Method -> String
writeMethod ut fcn =
  unlines
  [ "// ================== " ++ show (mKind fcn) ++ " method: " ++ show methodName' ++ " ==============="
  , "// class: " ++ show (TM.cppClassName ut)
  , "// cppName: " ++ show cppName
  , "// cWrapperName: " ++ show cWrapperName''
  , "// protoArgs: " ++ show protoArgs
  , "// args: " ++ show args
  , "// rettype: " ++ show retType
  , "// cWrapperRetType: " ++ show (TM.cWrapperRetType retType)
  , "// proto: " ++ show proto
  , "// call: " ++ show call
  , "// params: " ++ show (mParams fcn)
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , unlines marshals
  , TM.writeReturn retType call
  , "}"
  , ""
  ]
  where
    retType = mReturn fcn
    marshals = map (uncurry marshal) $ zip [0..] (mParams fcn)
    proto = TM.cWrapperRetType retType ++ " " ++ cWrapperName'' ++ protoArgs
    cWrapperName'' = TM.cWrapperName ut fcn
    cppName = TM.cppMethodName ut fcn
    Name methodName' = mName fcn
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (mParams fcn)
    allProtoArgs = case mKind fcn of
      Normal -> (TM.cppClassName ut ++ "* obj") : nonSelfProtoArgs
      _ -> nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $
                                    zip [0..] (mParams fcn)) ++ ")"
    call = case mKind fcn of
      Normal -> "obj->" ++ removeTics methodName' ++ args
      _ -> removeTics cppName ++ args
