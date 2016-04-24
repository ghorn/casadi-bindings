{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CallbackTests
       ( callbackTests
       ) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Casadi.Callback ( makeCallback )
import Casadi.DM ( DM )
import Casadi.Function ( callDM )
import Casadi.GenericType ( GType )
import Casadi.Sparsity ( dense )

simpleCallbackTest :: Test
simpleCallbackTest = testCase "simple callback test" $ HUnit.assert $ do
  let cb :: V.Vector DM -> M.Map String GType -> IO (V.Vector DM)
      cb xs stats = do
        putStrLn $ "stats: " ++ show stats
        case V.toList xs of
          [x] -> return (V.singleton (x*x))
          _ -> error $ "callback got wrong number of args: " ++ show xs

  fun <- makeCallback (V.singleton (dense 1 1)) (V.singleton (dense 1 1)) cb

  putStrLn "calling callback"
  outs <- callDM fun (V.singleton 2.2)
  putStrLn "finished calling callback"
  putStrLn $ "outputs: " ++ show outs

callbackTests :: Test
callbackTests =
  testGroup "callbacks"
  [ simpleCallbackTest
  ]
