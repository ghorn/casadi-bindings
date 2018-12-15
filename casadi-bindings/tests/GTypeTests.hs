{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTypeTests
       ( gtypeTests
       ) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Casadi.GenericType ( GenericType, GType(..), fromGType, toGType' )

gtypes :: [(String, GType)]
gtypes =
  gtypes' ++ gtypes'' ++ [("deeper map lol", GDict (M.fromList (gtypes' ++ gtypes'')))]
  where
    gtypes' :: [(String, GType)]
    gtypes' = gtypes'' ++ [("a map lol", GDict (M.fromList gtypes''))]

    doubleVec = V.fromList [42, 0, read "Infinity"]

    gtypes'' :: [(String, GType)]
    gtypes'' =
        [ ("GBool True", GBool True)
        , ("GBool False", GBool False)
        , ("GDouble 42", GDouble 42)
        , ("GDouble 0", GDouble 0)
        , ("GDouble read \"Infinity\"", GDouble (read "Infinity"))
        , ("GDouble read \"NaN\"", GDouble (read "NaN"))
        , ("GInt 42", GInt 42)
        , ("GInt 0", GInt 0)
        , ("GString \"\"", GString "")
        , ("GString \"yolo\"", GString "yolo")
          -- TODO(greg): re-enable these after "https://github.com/casadi/casadi/issues/1769" is fixed
--        , ("GBoolVec V.empty", GBoolVec V.empty)
--        , ("GBoolVec V.singleton True", GBoolVec (V.singleton True))
--        , ("GBoolVec V.fromList [True, False]", GBoolVec (V.fromList [True, False]))
        , ("GDoubleVec [42, 0, read \"Infinity\"]", GDoubleVec doubleVec)
        , ("GDoubleVecVec", GDoubleVecVec (V.fromList [doubleVec, V.singleton 22.2, doubleVec]))
        , ("GIntVec [42, 0]", GIntVec (V.fromList [42, 0]))
        , ("GIntVecVec", GIntVecVec (V.fromList [V.fromList [42, 0], V.singleton 2, V.empty]))
        , ("GStringVec [\"\", \"yolo\"]", GStringVec (V.fromList ["", "yolo"]))
--      --  , GFunction Function
        , ("GDict (M.fromList [])", GDict (M.fromList []))
        ]

testGType :: (String, GType) -> Test
testGType (name, gtype0) = testCase name $ HUnit.assert $ do
  gt <- fromGType gtype0 :: IO GenericType
  ego <- toGType' gt :: IO (Either String GType)
  case ego of
    Left msg -> HUnit.assertString $ "error converting from GenericType to GType: " ++ msg
    Right gtype1
      | gtype0 == gtype1 -> HUnit.assert True
      | otherwise -> HUnit.assertString $ init $ unlines
           [ "original GType doesn't equal new GType"
           , "original:"
           , show gtype0
           , "new:"
           , show gtype1
           ]

gtypeTests :: Test
gtypeTests =
  testGroup "GType conversion" (map testGType gtypes)
