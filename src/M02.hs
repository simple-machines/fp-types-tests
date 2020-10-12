{-# OPTIONS_GHC -Wall #-}

module M02 where

import Test.Framework(TestTree, testCase, testGroup, testProperty, test, (@?=), Arbitrary(arbitrary))
import Func(Func(runFunc))

{-
1. Remove: error "message"
2. enter code solution
-}
data Three a = Three a a a
  deriving (Eq, Show)

function1_M02 :: Three a -> Three a
function1_M02 = error "function1_M02 todo"

instance Arbitrary a => Arbitrary (Three a) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

prop_function1_M02_involution :: TestTree
prop_function1_M02_involution =
  testProperty "function1_M02 involution" (
    \t -> function1_M02 (function1_M02 t) == (t :: Three Int)
  )

function1_M02_test_00 :: TestTree
function1_M02_test_00 =
  testCase "function3_M01 True = True" (
    function1_M02 (Three 'a' 'b' 'c')
    @?=
    (Three 'c' 'b' 'a')
  )

----

data List x = Nil | Cons x (List x)
  deriving (Eq, Show)

contains :: Eq a => a -> List a -> Bool
contains _ Nil = False
contains x (Cons h t) = x == h || contains x t

-- returns true if all elements in the list
-- satisfy the given function
allElements :: (a -> Bool) -> List a -> Bool
allElements = error "allElements todo"

-- done for you, we can generate lists
instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    let r :: [a] -> List a
        r = foldr Cons Nil
    in  r <$> arbitrary

mapList :: (a -> b) -> List a -> List b
mapList = error "mapList todo"

-- is it possible to modify mapList such that this test fails?
-- "all elements in the result appear in the input"
prop_mapList_elements :: TestTree
prop_mapList_elements =
  testProperty "mapList: all elements in the result appear in the input" (
    (\f x ->
      let f' :: Func Int Int
          f' = f
      in  allElements (\e -> contains e x) (mapList (runFunc f') x)
    )
  )

len :: List a -> Int
len Nil = 0
len (Cons _ t) = 1 + len t

append :: List a -> List a -> List a
append Nil y = y
append (Cons h t) y = Cons h (append t y)

prop_append :: TestTree
prop_append =
  testProperty "append: appending two lists produces a length that is the sum of the length of operands" (
    (\x y ->
      let x' :: List Int
          x' = x
      in len x' + len y == len (x `append` y)
    )
  )

----

tests_M02 :: TestTree
tests_M02 =
  testGroup
    "all M02 tests"
    [
      prop_function1_M02_involution
    , function1_M02_test_00
    , prop_mapList_elements
    , prop_append
    ]

runTests_M02 :: IO ()
runTests_M02 = test tests_M02
