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

paramProto :: Int -> (Type, SwigOutput) -> String
paramProto k t = TM.cWrapperType t ++ " " ++ paramName k

-- todo: preserve constref if cWrapperType and cppMarshalType are the same
toMarshal :: Int -> (Type, SwigOutput) -> String
toMarshal k (t, SwigOutput False) =
  "        " ++ TM.cppMarshalType t ++ " " ++ paramName k ++ "_ = " ++
  "Marshaling<" ++ TM.cppMarshalType t ++ "," ++ TM.cWrapperType (t, SwigOutput False) ++ ">::marshal(" ++
  paramName k ++ ");"
toMarshal k (t, SwigOutput True) =
  "        " ++ TM.cppEmptySwigOutputType t ++ " " ++ paramName k ++ "_ = " ++
  TM.cppEmptySwigOutputType t ++ "();  // Swig output"
--  "EmptySwigOutput<" ++ TM.cppMarshalType t ++ ">::emptySwigOutput();"

toUnmarshal :: Int -> (Type, SwigOutput) -> String
toUnmarshal k (_, SwigOutput False) =
  "        // " ++ paramName k ++ " is not a swig output"
toUnmarshal k (t, SwigOutput True) =
  "        *" ++ paramName k ++ " = WrapReturn< " ++ TM.cWrapperType (t, SwigOutput False) ++ ", " ++ TM.cppEmptySwigOutputType t ++ " >::wrapReturn( " ++ paramName k ++ "_ );"
-- We use cppEmptySwigOutputType above because it has the right behavior,
-- but the name is misleading in toUnmarshal.
-- If cppEmptySwigOutputType changes, toUnmarshal may break here.

writeFunctions :: CppFunctions -> [String]
writeFunctions (Left fs) = map writeFunction fs
writeFunctions (Right f) = [writeFunction f]

errArg :: String
errArg = "std::string ** err_msg"

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
  , "// friendWrap: " ++ show (fFriendwrap fcn)
  , "// fName: " ++ show (fName fcn)
  , "// call: " ++ show call
  , "extern \"C\"\n" ++ proto ++ ";"
  , proto ++ "{"
  , "    try {"
  , unlines marshals
  , call
  , unlines unmarshals
  , TM.writeReturn retType
  , "    } catch (std::exception& ex) {"
  , "         *err_msg = new std::string(ex.what());"
  , "         " ++ errorReturn retType
  , "    }"
  , "}"
  , ""
  ]
  where
    retType = fReturn fcn
    params = fParams fcn
    marshals = map (uncurry toMarshal) $ zip [0..] params
    unmarshals = map (uncurry toUnmarshal) $ zip [0..] params
    proto = TM.cWrapperRetType retType ++ "\n    " ++ cWrapperName'' ++ protoArgs
    cWrapperName'' = TM.cWrapperName' fcn
    protoArgs = "(" ++ intercalate ", " (errArg : protoArgList) ++ ")"
    protoArgList = map (uncurry paramProto) $ zip [0..] params
    args = "(" ++ intercalate ", " args0 ++ ")"
    args0 = map ((++ "_"). paramName . fst) $ zip [0..] params
    cppName = "casadi::" ++ fName fcn

    call = case retType of
      CVoid -> "        " ++ call0 ++ ";"
      _ -> "        " ++ TM.cppType retType ++ " ret = " ++ call0 ++ ";"
    call0
      | fFriendwrap fcn = case (fName fcn, args0, retType, map fst params) of
        ("casadi_and", [x0, x1], _, _) -> x0 ++ " && " ++ x1
        ("casadi_eq", [x0, x1], _, _) -> x0 ++ " == " ++ x1
        ("casadi_ge", [x0, x1], _, _) -> x0 ++ " >= " ++ x1
        ("casadi_gt", [x0, x1], _, _) -> x0 ++ " > " ++ x1
        ("casadi_le", [x0, x1], _, _) -> x0 ++ " <= " ++ x1
        ("casadi_lt", [x0, x1], _, _) -> x0 ++ " < " ++ x1
        ("casadi_ldivide", [x0, x1], _, _) -> x0 ++ " / " ++ x1
        ("casadi_rdivide", [x0, x1], _, _) -> x0 ++ " / " ++ x1
        ("casadi_not", [x0], _, _) -> "!" ++ x0
        ("casadi_minus", [x0, x1], _, _) -> x0 ++ " - " ++ x1
        ("casadi_ne", [x0, x1], _, _) -> x0 ++ " != " ++ x1
        ("casadi_or", [x0, x1], _, _) -> x0 ++ " || " ++ x1
        ("casadi_plus", [x0, x1], _, _) -> x0 ++ " + " ++ x1
        ("casadi_times", [x0, x1], _, _) -> x0 ++ " * " ++ x1
        ("casadi_mod", [_, _], _, _) -> "fmod" ++ args
        ("casadi_min", [_, _], _, _) -> "fmin" ++ args
        ("casadi_max", [_, _], _, _) -> "fmax" ++ args
        ("casadi_power", [_, _], _, _) -> "pow" ++ args
        ("casadi_constpow", _, CDouble, [Ref (Const CDouble), Ref (Const CDouble)]) -> "casadi::constpow" ++ args
        ("casadi_erfinv", _, CDouble, [Ref (Const CDouble)]) -> "casadi::erfinv" ++ args
        ("casadi_simplify", _, CDouble, [Ref (Const CDouble)]) -> "casadi::simplify" ++ args
        ("casadi_sign", _, CDouble, [Ref (Const CDouble)]) -> "casadi::sign" ++ args
        ("casadi_is_equal", _, CBool, [Ref (Const CDouble), Ref (Const CDouble), CLongLong]) ->
          "casadi::is_equal" ++ args
        ("casadi_is_equal", _, CBool, [Ref (Const CDouble), Ref (Const CDouble)]) ->
          "casadi::is_equal" ++ args
        _ -> removeCasadi (fName fcn) ++ args
      | otherwise = removeTics cppName ++ args

    removeCasadi ('c':'a':'s':'a':'d':'i':'_':x) = x
    removeCasadi x = x

writeClass :: Class -> String
writeClass c = unlines $ writeDeletes ct : concatMap (writeMethods ct) (clMethods c)
  where
    ClassType ct = clType c

writeDeletes :: Type -> String
writeDeletes ut =
  unlines
  [ "// ================== delete "++ show ut ++"==============="
  , "// classType: " ++ show ut
  , "extern \"C\"\n" ++ proto ++ ";"
  , proto ++ "{"
  , "    delete obj;"
  , "}"
  ]
  where
    proto = "void " ++ TM.deleteName ut ++ "(" ++ TM.cWrapperType (ut, SwigOutput False) ++ " obj)"

removeTics :: String -> String
removeTics = reverse . removeTics' . reverse
  where
    removeTics' ('\'':xs) = removeTics' xs
    removeTics' x = x

writeMethods :: Type -> Methods -> [String]
writeMethods t (Left fs) = map (writeMethod t) fs
writeMethods t (Right f) = [writeMethod t f]

errorReturn :: Type -> String
errorReturn CVoid = "return;"
errorReturn _ = "return 0;"

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
  , "extern \"C\"\n" ++ proto ++ ";"
  , proto ++ "{"
  , "    try {"
  , unlines marshals
  , call
  , unlines unmarshals
  , case (retType, mKind fcn) of
     (UserType {}, Constructor) ->
       "        return ret;"
     _ -> TM.writeReturn retType
  , "    } catch (std::exception& ex) {"
  , "         *err_msg = new std::string(ex.what());"
  , "         " ++ errorReturn retType
  , "    }"
  , "}"
  , ""
  ]
  where
    retType = mReturn fcn
    marshals = map (uncurry toMarshal) $ zip [0..] (mParams fcn)
    unmarshals = map (uncurry toUnmarshal) $ zip [0..] (mParams fcn)
    proto = TM.cWrapperRetType retType ++ "\n    " ++ cWrapperName'' ++ protoArgs
    cWrapperName'' = TM.cWrapperName ut fcn
    cppName = TM.cppMethodName ut fcn
    Name methodName' = mName fcn
    protoArgs = "(" ++ intercalate ", " (errArg : allProtoArgs) ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (mParams fcn)
    allProtoArgs = case mKind fcn of
      Normal -> (TM.cppClassName ut ++ "* obj") : nonSelfProtoArgs
      _ -> nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $
                                    zip [0..] (mParams fcn)) ++ ")"

    call = case (retType, mKind fcn) of
      (CVoid, _) -> "        " ++ call0 ++ ";"
      (UserType {}, Constructor) ->
        "        " ++ TM.cWrapperRetType retType ++ " ret = " ++
        "(" ++ TM.cWrapperRetType retType++ ")new " ++ call0 ++ ";"
      _ -> "        " ++ TM.cppType retType ++ " ret = " ++ call0 ++ ";"

    call0 = case mKind fcn of
      Normal -> "obj->" ++ removeTics methodName' ++ args
      _ -> removeTics cppName ++ args
