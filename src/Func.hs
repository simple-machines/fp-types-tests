{-# OPTIONS_GHC -Wall #-}

module Func where

import Test.Framework(Arbitrary(arbitrary), Coarbitrary)

data Func a b = Func { runFunc :: a -> b }

-- we can print out functions
-- required for our tests
instance Show (Func a b) where
  show _ = "<func>"

instance (Coarbitrary a, Arbitrary b) => Arbitrary (Func a b) where
  arbitrary = Func <$> arbitrary
