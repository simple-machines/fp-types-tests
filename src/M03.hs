{-# OPTIONS_GHC -Wall #-}

module M03 where

import Test.Framework(TestTree, testGroup, testProperty, test)
import Func
import M02

class Maps k where
  maps :: (a -> b) -> k a -> k b

instance Maps List where
  maps = error "List#maps todo"

-- is it possible to write a bug into maps that fails this test?
prop_identity :: TestTree
prop_identity =
  testProperty "List#maps identity" (
    \x -> let x' :: List Int
              x' = x
          in  maps (\e -> e) x == x'
  )

prop_composition :: TestTree
prop_composition =
  testProperty "List#maps composition" (
    \f g x -> let f' :: Int -> Int
                  f' = runFunc f
                  g' :: Int -> Int
                  g' = runFunc g
                  x' :: List Int
                  x' = x
              in  maps (\e -> f' (g' e)) x == maps f' (maps g' x')
  )

instance Maps (Func x) where
  maps =
    error "(Func x)#maps todo"

prop_identity2 :: TestTree
prop_identity2 =
  testProperty "(Func x)#maps identity" (
    \x a -> let x' :: Func Int Int
                x' = x
            in  runFunc (maps (\e -> e) x) a == runFunc x' a
  )


-- more tests?

anonymousMaps :: Maps k => a -> k b -> k a
anonymousMaps = error "anonymousMaps todo"

-- tests?

----

tests_M03 :: TestTree
tests_M03 =
  testGroup
    "all M03 tests"
    [
      prop_identity
    , prop_composition
    , prop_identity2
    ]

runTests_M03 :: IO ()
runTests_M03 = test tests_M03
