{-# LANGUAGE TemplateHaskell #-}

import           Data.Monoid                          (mempty)
import           Test.Framework                       (buildTest, defaultMain,
                                                       defaultMainWithOpts,
                                                       testGroup)
import           Test.Framework.Options               (TestOptions,
                                                       TestOptions' (..))
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.Runners.Options       (RunnerOptions,
                                                       RunnerOptions' (..))
import           Test.Framework.TH

import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad                        (liftM)
import           Control.Monad.Trans                  (lift)
import           Data.List
import           Data.UnorderedMap

main = $(defaultMainGenerator)

prop_map1 xs = monadicIO . run $ do
  xs' <- fromList xs >>= toList
  return (sort xs == sort xs')
    where types = (xs :: [(Int, String)])
