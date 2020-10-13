{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module M01 where

import Test.Framework(Arbitrary(arbitrary), TestTree, testCase, testGroup, test, (@?=))
import Data.Foldable(traverse_)

{-
1. Remove: error "message"
2. enter code solution
-}

-- return type to the power of the argument type
-- Int^Int
-- (2^29)^(2^29)
-- Always adds 1 to its argument
function1_M01 :: Int -> Int
function1_M01 = \n -> n + 1

function1_M01_test_blah :: Int -> TestTree
function1_M01_test_blah x =
  testCase "function1_M01 x = x + 1" (
    function1_M01 x
    @?=
    (x + 1)
  )

function1_M01_test :: IO ()
function1_M01_test =
  traverse_ (\n -> test (function1_M01_test_blah n)) [minBound .. maxBound]

function1_M01_test_00 :: TestTree
function1_M01_test_00 =
  testCase "function1_M01 99 = 100" (
    function1_M01 99
    @?=
    (99 + 1)
  )

function1_M01_test_01 :: TestTree
function1_M01_test_01 =
  testCase "function1_M01 399 = 400" (
    function1_M01 399
    @?=
    400
  )

----

-- (2^29)^2
-- (2^29)^2 - 5
function2_M01 :: Bool -> Int
function2_M01 = \_ -> 100

function2_M01_test_00 :: TestTree
function2_M01_test_00 =
  testCase "function2_M01 True = 100" (
    function2_M01 True
    @?=
    100
  )

function2_M01_test_01 :: TestTree
function2_M01_test_01 =
  testCase "function2_M01 False = 100" (
    function2_M01 False
    @?=
    100
  )

----

-- Bool^Bool
function3_M01 :: Bool -> Bool
function3_M01 a = a

{-
function3_M01 :: Bool -> Bool
function3_M01 a = a
-}

-- forall a. function3_M01 a == a

function3_M01_test_00 :: TestTree
function3_M01_test_00 =
  testCase "function3_M01 True = True" (
    function3_M01 True
    @?=
    True
  )

function3_M01_test_01 :: TestTree
function3_M01_test_01 =
  testCase "function3_M01 False = False" (
    function3_M01 False
    @?=
    False
  )

----

-- type-casing
-- is this value of some type?
-- instanceof
-- typeCase :: a -> Bool

-- type-casting
-- convert type T to U
-- (U)t

-- uninhabited type
-- 0 programs have this type
{-typeCast :: a -> b
typeCast = 
-}
-- 4
-- YOU CAN'T DO ANYTHING WITH a
function4_M01 :: a -> Bool -> Bool
function4_M01 = \_ -> id

data I = I (Int -> Int)

{-
class NoActuallyObject {
  String toString() { throw WTF(""); } 
-}

instance Show I where
  show (I _) = "hi"


class ToString a where
  toString :: a -> String

instance ToString Bool where
  toString True = "True"
  toString False = "False"

-- trap :: a -> b

-- 1
k :: a -> b -> a
k = \a _ -> a

-- if you see lots of polymorphism (generics)
-- type holes will help you a lot

-- if you have a type hole, and that type hole is a function
-- and you are stuck
-- type this: \name -> _ (and then :r)
-- Remember: check Relevant bindings and Valid holes
-- ANY compiling program will do

-- 1
-- s = (<*>)
s :: (t -> a -> b) -> (t -> a) -> t -> b
s = \f -> \g -> \x -> f x (g x)

function4_M01_test_00 :: TestTree
function4_M01_test_00 =
  testCase "function4_M01 99 True = True" (
    function4_M01 99 True
    @?=
    True
  )

function4_M01_test_01 :: TestTree
function4_M01_test_01 =
  testCase "function4_M01 100 False = False" (
    function4_M01 100 False
    @?=
    False
  )

-- what are we going to write here?
function4_M01_test_02 :: TestTree
function4_M01_test_02 =
  error "function4_M01_test_02"

----

-- == Bool
function5_M01 :: a -> a -> a
function5_M01 = \a _ -> a
             -- \_ a -> a

function5_M01_test_00 :: TestTree
function5_M01_test_00 =
  error "function5_M01_test_00"

function5_M01_test_01 :: TestTree
function5_M01_test_01 =
  error "function5_M01_test_01"

-- "cons list"
data LinkedList a = Empty | Prepend a (LinkedList a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (LinkedList a) where
  arbitrary =
    let fl [] = Empty
        fl (h:t) = Prepend h (fl t)
    in  fl <$> arbitrary

-- ? infinity

-- "all elements in the result list, appear in the input list"
thinkingOfAFunction :: LinkedList a -> LinkedList a
thinkingOfAFunction x = 
  let reverse0 :: LinkedList a -> LinkedList a -> LinkedList a
      reverse0 Empty acc = acc
      reverse0 (Prepend h t) acc = reverse0 t (Prepend h acc)
  in  reverse0 x Empty

appendLinkedList :: LinkedList a -> LinkedList a -> LinkedList a
appendLinkedList Empty y = y
appendLinkedList (Prepend h t) y = Prepend h (appendLinkedList t y)

-- test: "there does not exist a failed proof (counter-example)"
-- type: proof true

test1 :: Eq a => a -> Bool
test1 x =
  thinkingOfAFunction (Prepend x Empty) == Prepend x Empty

-- test: "there (not) exists..." existential quantification
-- type: "for all..." universal quantification
test2 :: Eq a => LinkedList a -> LinkedList a -> Bool
test2 x y =
  thinkingOfAFunction (appendLinkedList x y) ==
  appendLinkedList (thinkingOfAFunction y) (thinkingOfAFunction x)

{-

forall x. thinkingOfAFunction (Prepend x Empty) = Prepend x Empty

forall x y. thinkingOfAFunction (appendLinkedList x y) ==
            appendLinkedList (thinkingOfAFunction y) (thinkingOfAFunction x)

-}


data DoubleList a = DoubleList (LinkedList a -> LinkedList a)

-- snoc puts on the end
-- O(1)
snoc :: DoubleList a -> a -> DoubleList a
snoc = error "have fun!"

-- cons put on the front
-- O(1)
cons :: a -> DoubleList a -> DoubleList a
cons = error "have fun!"

-- headL O(n)
headL :: DoubleList a -> Maybe a
headL = error "have fun!"

----

tests_M01 :: TestTree
tests_M01 =
  testGroup
    "all M01 tests"
    [
      function1_M01_test_00
    , function1_M01_test_01
    , function2_M01_test_00
    , function2_M01_test_01
    , function3_M01_test_00
    , function3_M01_test_01
    , function4_M01_test_02
    , function5_M01_test_00
    , function5_M01_test_01
    ]

runTests_M01 :: IO ()
runTests_M01 = test tests_M01
