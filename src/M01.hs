{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module M01 where

import Test.Framework(TestTree, testCase, testGroup, test, (@?=))

{-
1. Remove: error "message"
2. enter code solution
-}

function1_M01 :: Int -> Int
function1_M01 = error "function1_M01 todo"

function1_M01_test_00 :: TestTree
function1_M01_test_00 =
  testCase "function1_M01 99 = 100" (
    function1_M01 99
    @?=
    100
  )

----

function2_M01 :: Bool -> Int
function2_M01 = error "function2_M01 todo"

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

function3_M01 :: Bool -> Bool
function3_M01 = error "function3_M01 todo"

function3_M01_test_00 :: TestTree
function3_M01_test_00 =
  testCase "function3_M01 True = True" (
    function3_M01 True
    @?=
    True
  )

function3_M01_test_01 :: TestTree
function3_M01_test_01 =
  error "function3_M01_test_01"

----

function4_M01 :: a -> Bool -> Bool
function4_M01 = error "function4_M01 todo"

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

function5_M01 :: a -> a -> a
function5_M01 = error "function5_M01 todo"

function5_M01_test_00 :: TestTree
function5_M01_test_00 =
  error "function5_M01_test_00"

function5_M01_test_01 :: TestTree
function5_M01_test_01 =
  error "function5_M01_test_01"

----

tests_M01 :: TestTree
tests_M01 =
  testGroup
    "all M01 tests"
    [
      function1_M01_test_00
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
