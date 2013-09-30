{-# OPTIONS_GHC -Wall #-}

module WriteCasadiBindings.WriteC ( writeFunction
                                  , writeClass
                                  , writeDeletes
                                  ) where

import Data.List ( intercalate )

import WriteCasadiBindings.Types
import WriteCasadiBindings.TypeMaps

paramName :: Int -> String
paramName k = "x" ++ show k

paramProto :: Int -> Type -> String
paramProto k t = cWrapperType t ++ " " ++ paramName k


-- todo: preserve constref if cWrapperType and cppMarshalType are the same
marshal :: Int -> Type -> String
marshal k t = "    " ++ cppMarshalType t ++ " " ++ paramName k ++
              "_ = Marshaling<" ++ cppMarshalType t ++ "," ++ cWrapperType t ++ ">::marshal(" ++
              paramName k ++ ");"



writeFunction :: Function -> String
writeFunction fcn@(Function (Name functionName) retType params _) =
  unlines
  [ "// ================== function " ++ show functionName ++ " ==============="
  , "// cppName: " ++ show cppName
  , "// cWrapperName: " ++ show cWrapperName''
  , "// protoArgs: " ++ show protoArgs
  , "// params: " ++ show params
  , "// args: " ++ show args
  , "// cWrapperRetType: " ++ show (cWrapperRetType retType)
  , "// proto: " ++ show proto
  , "// call: " ++ show call
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , unlines marshals
  , writeReturn retType call
  , "}"
  , ""
  ]
  where
    marshals = map (uncurry marshal) $ zip [0..] params
    proto = cWrapperRetType retType ++ " " ++ cWrapperName'' ++ protoArgs
    cWrapperName'' = cWrapperName' fcn
    cppName = functionName
    protoArgs = "(" ++ intercalate ", " protoArgList ++ ")"
    protoArgList = map (uncurry paramProto) $ zip [0..] params
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] params) ++ ")"
    call = removeTics cppName ++ args

writeClass :: Class -> [String]
writeClass (Class classType methods _) =
  writeDeletes (CasadiClass classType) : map (writeMethod classType) methods

writeDeletes :: Primitive -> String
writeDeletes classType =
  unlines
  [ "// ================== delete "++ show classname ++"==============="
  , "// classname: " ++ show classname
  ] ++ concatMap writeIt types
  where
    classname = cppTypePrim classType

    types = [ NonVec classType
            , Vec (NonVec classType)
            , Vec (Vec (NonVec classType))
            , Vec (Vec (Vec (NonVec classType)))
            ]
    writeIt c =
      unlines $
      [ "extern \"C\"\n    " ++ proto ++ ";"
      , proto ++ "{"
      , "    delete obj;"
      , "}"
      ]
      where
        proto = "void " ++ (deleteName c) ++ "(" ++ cppTypeTV c ++ "* obj)"

removeTics :: String -> String
removeTics = reverse . removeTics' . reverse
  where
    removeTics' ('\'':xs) = removeTics' xs
    removeTics' x = x

writeMethod :: CasadiClass -> Method -> String
writeMethod classType fcn =
  unlines
  [ "// ================== " ++ show (fMethodType fcn) ++ " method: " ++ show methodName ++ " ==============="
  , "// class: " ++ show (cppClassName classType)
  , "// cppName: " ++ show cppName
  , "// cWrapperName: " ++ show cWrapperName''
  , "// protoArgs: " ++ show protoArgs
  , "// args: " ++ show args
  , "// rettype: " ++ show retType
  , "// cWrapperRetType: " ++ show (cWrapperRetType retType)
  , "// proto: " ++ show proto
  , "// call: " ++ show call
  , "extern \"C\"\n    " ++ proto ++ ";"
  , proto ++ "{"
  , unlines marshals
  , writeReturn (fType fcn) call
  , "}"
  , ""
  ]
  where
    retType = fType fcn
    marshals = map (uncurry marshal) $ zip [0..] (fArgs fcn)
    proto = cWrapperRetType retType ++ " " ++ cWrapperName'' ++ protoArgs
    cWrapperName'' = cWrapperName classType fcn
    cppName = cppMethodName classType fcn
    Name methodName = fName fcn
    protoArgs = "(" ++ intercalate ", " allProtoArgs ++ ")"
    nonSelfProtoArgs = map (uncurry paramProto) $ zip [0..] (fArgs fcn)
    allProtoArgs = case fMethodType fcn of
      Normal -> (cppClassName classType ++ "* obj") : nonSelfProtoArgs
      _ -> nonSelfProtoArgs
    args = "(" ++ intercalate ", " (map ((++ "_"). paramName . fst) $ zip [0..] (fArgs fcn)) ++ ")"
    call = case fMethodType fcn of
      Normal -> "obj->" ++ removeTics methodName ++ args
      _ -> removeTics cppName ++ args
