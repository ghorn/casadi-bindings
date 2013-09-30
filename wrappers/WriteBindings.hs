{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified WriteCasadiBindings.WriteC as C
import qualified WriteCasadiBindings.WriteHs as HS
import WriteCasadiBindings.CasadiTree
import WriteCasadiBindings.CasadiClasses
import WriteCasadiBindings.Types
import WriteCasadiBindings.WriteForeignTools

main :: IO ()
main = do
  let cOut = init $ unlines $
             [ "#include <build/swig/swiginclude.hpp>"
             , "#include \"../marshal.hpp\""
             ] ++
             concatMap C.writeClass classes' ++
             map C.writeFunction tools' ++ map C.writeDeletes [CInt,CDouble,StdString,CBool]
      hsDeleters = HS.writeDeleterModule classes'
      hsData = HS.writeDataModule classes' inheritance
      hsClassModules = HS.writeClassModules inheritance classes'
      hsToolsModule = HS.writeToolsModule tools'

  writeFile "Casadi/Wrappers/ForeignToolsImports.hs" foreignToolsImports
  writeFile "Casadi/Wrappers/ForeignToolsInstances.hs" foreignToolsInstances

  writeFile "cbits/autogen/all.cpp" cOut
  writeFile "Casadi/Wrappers/Data.hs" hsData
  writeFile "Casadi/Wrappers/Deleters.hs" hsDeleters
  mapM_ (\(dataname, src) -> writeFile ("Casadi/Wrappers/" ++ dataname ++ ".hs") src)  hsClassModules
  writeFile "Casadi/Wrappers/Tools.hs" hsToolsModule
  writeFile "Casadi/Wrappers/modules.txt" $
    unlines $ map ((\(dataname,_) -> "Casadi.Wrappers." ++ dataname)) hsClassModules

tools' :: [Function]
tools' = map addNamespace $ filter (not . hasStdOstream) tools
  where
    addNamespace :: Function -> Function
    addNamespace (Function (Name name) x y z) = Function (Name ("CasADi::"++name)) x y z


classes' :: [Class]
classes' = map filterStdOstreams classes

filterStdOstreams :: Class -> Class
filterStdOstreams (Class cc methods docs) = Class cc methods' docs
  where
    methods' = filter (not . hasStdOstream') methods

-- remove methods with StdOStrea'
hasStdOstream' :: Method -> Bool
hasStdOstream' (Method _ ret params _ _) = StdOstream `elem` (map getPrim (ret:params))

-- remove methods with StdOStrea'
hasStdOstream :: Function -> Bool
hasStdOstream (Function _ _ params _) = StdOstream `elem` (map getPrim params)

getPrim :: Type -> Primitive
getPrim (Val x) = getPrimTV x
getPrim (Ref x) = getPrimTV x
getPrim (ConstRef x) = getPrimTV x

getPrimTV :: ThreeVectors -> Primitive
getPrimTV (NonVec x) = x
getPrimTV (Vec (NonVec x)) = x
getPrimTV (Vec (Vec (NonVec x))) = x
getPrimTV (Vec (Vec (Vec (NonVec x)))) = x
getPrimTV (Vec (Vec (Vec (Vec ())))) = error "getPrimTV: Vec (Vec (Vec (Vec ())))"
