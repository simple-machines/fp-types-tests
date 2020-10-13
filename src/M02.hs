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

-- 1. implementation that fails both tests below
-- 2. implementation that fails only the second test
-- 3. implementation that fails neither test

-- proof by parametricity
-- polymorphism improves code readability (by not a little bit)
function1_M02 :: Three a -> Three a
function1_M02 = \(Three a b c) -> Three c b a

function1_M02_Again :: Three Char -> Three Char
function1_M02_Again (Three 'þ' '®' '¥') = Three 'x' 'y' 'z'
function1_M02_Again (Three a b c) = Three c b a
-- ?

-- hint: tests are not proof
prop_function1_M02_reverses_Again :: TestTree
prop_function1_M02_reverses_Again =
  testProperty "reverses" (
    \a b c ->
      let _ = a :: Char
      in  function1_M02_Again (Three a b c) == Three c b a
  )

-- 2^(29*29*29)^2^(29*29*29)
-- function1_M02' :: Three Int -> Three Int
-- function1_M02' = ...

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
    function1_M02 (Three 'a' 'a' 'a')
    @?=
    (Three 'a' 'a' 'a')
  )

prop_function1_M02_reverses :: TestTree
prop_function1_M02_reverses =
  testProperty "reverses" (
    \a b c ->
      let _ = a :: Char
      in  function1_M02 (Three a b c) == Three c b a
  )

-- write a unit test or test property, such that the malicious programmer
-- cannot introduce a bug
-- it should reverse the elements (or your test will fail)

mapThree :: (a -> b) -> Three a -> Three b
mapThree = error "todo"

prop_mapThree_identity :: TestTree
prop_mapThree_identity =
  testProperty "mapThree identity" (
    \x ->
      let x' :: Three Int
          x' = x
      in  mapThree (\e -> e) x' == x
  )
prop_mapThree_composition :: TestTree
prop_mapThree_composition =
  testProperty "mapThree composition" (
    \f g x ->
      let f' :: Int -> Int
          f' = runFunc f
          g' :: Int -> Int
          g' = runFunc g
      in  mapThree (\e -> f' (g' e)) x ==
          mapThree f' (mapThree g' x)
  )

{-
freeTheorem =
  testProperty "free theorem" (
    \f r x ->
      let r' :: Three Int -> Three Int
          r' = r
      in  (mapThree f . r') x == (r' . mapThree f) x
  )
-}


----

data List x = Nil | Cons x (List x)
  deriving (Eq, Show)

contains :: Eq a => a -> List a -> Bool
contains _ Nil = False
contains x (Cons h t) = x == h || contains x t

-- returns true if all elements in the list
-- satisfy the given function
allElements :: (a -> Bool) -> List a -> Bool
allElements _ Nil = True
allElements p (Cons h t) = p h && allElements p t

anyElements :: (a -> Bool) -> List a -> Bool
anyElements _ Nil = False
anyElements p (Cons h t) = p h || allElements p t

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil = b
foldRight f b (Cons h t) = f h (foldRight f b t)

-- done for you, we can generate lists
instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    let r :: [a] -> List a
        r = foldr Cons Nil
    in  r <$> arbitrary

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons h t) = f h `Cons` mapList f t

-- is it possible to modify mapList such that this test fails?
-- "all elements in the result appear in the input (after having the function applied)"
prop_mapList_elements :: TestTree
prop_mapList_elements =
  testProperty "mapList: all elements in the result appear in the input" (
    (\f x ->
      let f' :: Func Int Int
          f' = f
      in  allElements (\e -> contains e x) (mapList (runFunc f) x)
    )
  )

letsfigureitout =
  let x = Cons (-1) (Cons 1 (Cons 3 Nil))
      f = (+1)
  in  allElements (\e -> anyElements (\v -> e == f v) x) (mapList (id f) x)

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
