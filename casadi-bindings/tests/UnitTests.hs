{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import qualified Data.Monoid as Mo
import Test.Framework
       ( Test, ColorMode(..), RunnerOptions'(..), TestOptions'(..)
       , defaultMainWithOpts )

import GTypeTests ( gtypeTests )
import CallbackTests ( callbackTests )

main :: IO ()
main = defaultMainWithOpts tests opts

tests :: [Test]
tests = 
  [ gtypeTests
  , callbackTests
  ]

opts :: RunnerOptions' Maybe
opts =
  Mo.mempty
  { ropt_color_mode = Just ColorAlways
  , ropt_threads = Just 1
  , ropt_test_options = Just my_test_opts
  }

my_test_opts :: TestOptions' Maybe
my_test_opts =
  Mo.mempty
  { topt_timeout = Just (Just 15000000)
  }
