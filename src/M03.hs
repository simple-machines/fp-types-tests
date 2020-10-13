{-# OPTIONS_GHC -Wall #-}

module M03 where

import Test.Framework(TestTree, testGroup, testProperty, test)
import Func
import M02

data Compared = LessThan | EqualTo | GreaterThan

class OrderOf a where
  compareOrder :: a -> a -> Compared

instance OrderOf Int where
  compareOrder x y = if x < y then LessThan else if x > y then GreaterThan else EqualTo

instance OrderOf Char where
  compareOrder x y = if x < y then LessThan else if x > y then GreaterThan else EqualTo

sortList :: OrderOf a => List a -> List a
sortList = error "blah sort *all* the things, once and for all"

sortListInt :: List Int -> List Int
sortListInt = error "blah sorting stuff here"

sortListChar :: List Char -> List Char
sortListChar = error "blah sorting stuff here"

-- etc etc

-- interface

-- all things for k must have kind :: * -> *
-- agree?
class Maps k where
  maps :: (a -> b) -> k a -> k b

-- implementation (List)
instance Maps List where
--maps :: (a -> b) -> List a -> List b
  maps _ _ = Nil

-- instance Maps Int where
-- "type of type" error
-- kind error
-- "type" "type of value"
--maps :: (a -> b) -> Int a -> Int b

-- is it possible to write a bug into maps that fails this test?

-- if the law of composition holds, then the law of identity holds
-- true for anything that maps (not just List)
-- free theorem

-- the identity law
prop_identity :: TestTree
prop_identity =
  testProperty "List#maps identity" (
    \x -> let x' :: List Int
              x' = x
          in  maps (\e -> e) x == x'
  )

-- the composition law
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
  -- = 1
  -- :kind Func
  -- (a -> b) -> Func x a -> Func x b
  maps =
    -- type holes
    \f -> \g -> Func (\x -> f (runFunc g x))

instance Maps ((->) x) where
  -- (a -> b) -> ((->) x) a -> ((->) x) b
  -- (a -> b) -> (->) x a -> (->) x b
  --      (a -> b) -> (x -> a) -> x -> b
  -- (x -> a -> b) -> (x -> a) -> x -> b
  -- = 1
  -- type holes
  maps =
    \f -> \g -> \x -> f (g x)

prop_identity2 :: TestTree
prop_identity2 =
  testProperty "(Func x)#maps identity" (
    \x a -> let x' :: Func Int Int
                x' = x
            in  runFunc (maps (\e -> e) x) a == runFunc x' a
  )


-- more tests?


-- maps :: (x -> y) -> k x -> k y
-- maps :: (b -> a) -> k b -> k a

anonymousMaps :: Maps k => a -> k b -> k a
-- maps ::                    (x -> y) -> k x -> k y
-- maps ::                    (b -> a) -> k b -> k a
anonymousMaps = \a kb -> maps (\_ -> a) kb

-- if you have a type hole, and that type hole is a function
-- and you are stuck
-- type this: \name -> _ (and then :r)
-- Remember: check Relevant bindings and Valid holes
-- ANY compiling program will do

-- 
-- shall we write tests?
-- if so why?
-- if not why not?


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
