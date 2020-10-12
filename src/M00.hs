{-# OPTIONS_GHC -Wall #-}

module M00 where

x :: Integer
x = 99

func1 :: Integer -> Integer
func1 a = a + 10

-- (->) is right-associative

-- first letter in func2 matches ['a'..'z']
func2 :: Integer -> (Integer -> Integer)
func2 a b = (a + b) * 2

-- first letter in (.+.) does not match ['a'..'z']
(.+.) :: Integer -> (Integer -> Integer)
(.+.) a b = (a + b) * 2


func1Again :: Integer -> Integer
func1Again = \a -> a + 10 -- lambda expression

func3 :: (Integer -> t) -> t
func3 k = k 99

func4 :: m -> k -> m
func4 = \a    _ -> a

-- in Java/C# constructors always have the same name as the data type
-- in Haskell, they might but don't have to
data ThreeIntegers = SomethingElse Integer Integer Integer
  deriving (Eq, Show)

name1 :: ThreeIntegers
name1 = SomethingElse 44 55 66

-- addThreeIntegers should add 3 Integers
addThreeIntegers :: ThreeIntegers -> Integer
-- pattern-matching with case/of
addThreeIntegers = \three -> case three of SomethingElse a b c -> a + b + c

addThreeIntegersAgain :: ThreeIntegers -> Integer
addThreeIntegersAgain (SomethingElse a b c) = a + b + c

-- class ThreeAnything<t> { ThreeAnything(t t1, t t2, t t3)
data ThreeAnything t = ThreeAnything t t t
  deriving (Eq, Show)

addThreeAnythingInteger :: ThreeAnything Integer -> Integer
addThreeAnythingInteger (ThreeAnything a b c) = a + b + c

-- malicious programmer will try to introduce bug
-- however, they cannot change the type
-- what damage can the malicious programmer do?
-- two other things
firstValue :: ThreeAnything t -> t
firstValue (ThreeAnything t1 _ _) = t1

-- traditional unit testing
testFirstValue :: Bool
testFirstValue = firstValue (ThreeAnything 1 2 3) == (1 :: Integer)

data Shape =
  Circle Integer | Rectangle Integer Integer | Triangle Integer Integer Integer
  deriving (Eq, Show)

pie :: Integer
pie = 3

perimeter :: Shape -> Integer
perimeter = \s -> case s of
  Circle r -> 2 * r * pie
  Rectangle w h -> (w + h) * 2
  Triangle a b c -> a + b + c

perimeterAgain :: Shape -> Integer
perimeterAgain = "abc"

-- if the type tells us the program, then why write the program?

-- \given these things then return -> this thing

{-
Haskell syntax:

* GHCi tools
  * :reload
  * :reload!
  * :type
  * :info
* Ensure editor TAB spacing is turned OFF
* values, assignment
* type signatures `::` reads as *has the type*
  * The `->` in a type signature is *right-associative*
* functions are values
* functions take arguments
  * functions take *only one argument* but we approximate with spoken
    language
  * functions can be declared inline using *lambda expressions*
  * the `\` symbol in a lambda expression denotes a Greek lambda
* operators, beginning with non-alpha character, are in infix position by
  default
  * use in prefix position by surrounding with *(parentheses)*
* regular identifiers, beginning with alpha character, are in prefix position by
  default
  * use in infix position by surrounding with ``backticks``
* polymorphism
  * type variables *always* start with a lower-case character
* data types, declared using the `data` keyword
  * following the `data` keyword is the *data type name*
  * following the data type name are zero of more type variables
  * then `=` sign
  * data types have zero or more constructors
    * data type constructors start with an upper-case character, or colon `(:)`
  * following each constructor is a list of zero or more *constructor arguments*
  * between each constructor is a pipe symbol `(|)`
  * the `deriving` keyword gives us default implementations for some functions
    on that data type
  * when constructors appear on the left side of `=` we are *pattern-matching*
  * when constructors appear on the right side of `=` we are *constructing*
* type-classes
-}

