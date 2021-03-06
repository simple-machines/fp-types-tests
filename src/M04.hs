{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

module M04 where

data Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

lowerYoneda :: Yoneda f a -> f a
lowerYoneda y = runYoneda y id

lowerYoneda' :: (forall b. (a -> b) -> f b) -> f a
lowerYoneda' f = lowerYoneda (Yoneda f)

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda a = Yoneda (\f -> fmap f a)

liftYoneda' :: Functor f => f a -> (a -> b) -> f b
liftYoneda' = runYoneda . liftYoneda

---- id :: a -> a ----

id_unit_fw :: (forall a. a -> a) -> ()
id_unit_fw f = step_4 (step_3 (step_2 (step_1 f)))
  where
    step_1 :: (forall a. a -> a) -> (forall a. (() -> a) -> a)
    step_1 f' g = f' (g ())

    step_2 :: (forall a. (() -> a) -> a) -> (forall a. (() -> a) -> Identity a)
    step_2 = (Identity .)

    step_3 :: (forall a. (() -> a) -> Identity a) -> Identity ()
    step_3 = lowerYoneda'

    step_4 :: Identity () -> ()
    step_4 = runIdentity

id_unit_bw :: () -> (forall a. a -> a)
id_unit_bw x = step_4 (step_3 (step_2 (step_1 x)))
  where
    step_1 :: () -> Identity ()
    step_1 = Identity

    step_2 :: Identity () -> (forall a. (() -> a) -> Identity a)
    step_2 = liftYoneda'

    step_3 :: (forall a. (() -> a) -> Identity a) -> (forall a. (() -> a) -> a)
    step_3 = (runIdentity .)

    step_4 :: (forall a. (() -> a) -> a) -> (forall a. a -> a)
    step_4 f a = f (\() -> a)

---- (.) :: (b -> c) -> (a -> b) -> a -> c ----

compose_unit_fw :: (forall a b c. (b -> c) -> (a -> b) -> a -> c) -> ()
compose_unit_fw f = step_5 (step_4 (step_3 (step_2 (step_1 f))))
  where
    step_1
      :: (forall a b c. (b -> c) -> (a -> b) -> a -> c)
      -> (forall a b c. (b -> c) -> (a -> b, a) -> c)
    step_1 = (uncurry .)

    step_2
      :: (forall a b c. (b -> c) -> (a -> b, a) -> c)
      -> (forall a b. (a -> b, a) -> b)
    step_2 = lowerYoneda'

    step_3
      :: (forall a b. (a -> b, a) -> b)
      -> (forall a b. (a -> b) -> a -> b)
    step_3 = curry

    step_4
      :: (forall a b. (a -> b) -> a -> b)
      -> (forall a. a -> a)
    step_4 = lowerYoneda'

    step_5 :: (forall a. a -> a) -> ()
    step_5 = id_unit_fw

compose_unit_bw :: () -> (forall a b c. (b -> c) -> (a -> b) -> a -> c)
compose_unit_bw x = step_5 (step_4 (step_3 (step_2 (step_1 x))))
  where
    step_1 :: () -> (forall a. a -> a)
    step_1 = id_unit_bw

    step_2
      :: (forall a. a -> a)
      -> (forall a b. (a -> b) -> a -> b)
    step_2 = liftYoneda'

    step_3
      :: (forall a b. (a -> b) -> a -> b)
      -> (forall a b. (a -> b, a) -> b)
    step_3 = uncurry

    step_4
      :: (forall a b. (a -> b, a) -> b)
      -> (forall a b c. (b -> c) -> (a -> b, a) -> c)
    step_4 = liftYoneda'

    step_5
      :: (forall a b c. (b -> c) -> (a -> b, a) -> c)
      -> (forall a b c. (b -> c) -> (a -> b) -> a -> c)
    step_5 = (curry .)

compose :: (b -> c) -> (a -> b) -> a -> c
compose = compose_unit_bw ()


---- a -> a -> a ----
bool_fw :: (forall a. a -> a -> a) -> Bool
bool_fw f = step_5 (step_4 (step_3 (step_2 (step_1 f))))
  where
    step_1 :: (forall a. a -> a -> a) -> (forall a. (a, a) -> a)
    step_1 = uncurry

    step_2 :: (forall a. (a, a) -> a) -> (forall a. (Bool -> a) -> a)
    step_2 f' g = f' (g True, g False)

    step_3 :: (forall a. (Bool -> a) -> a) -> (forall a. (Bool -> a) -> Identity a)
    step_3 = (Identity .)

    step_4 :: (forall a. (Bool -> a) -> Identity a) -> Identity Bool
    step_4 = lowerYoneda'

    step_5 :: Identity Bool -> Bool
    step_5 = runIdentity

bool_bw :: Bool -> (forall a. a -> a -> a)
bool_bw x = step_5 (step_4 (step_3 (step_2 (step_1 x))))
  where
    step_1 :: Bool -> Identity Bool
    step_1 = Identity

    step_2 :: Identity Bool -> (forall a. (Bool -> a) -> Identity a)
    step_2 = liftYoneda'

    step_3 :: (forall a. (Bool -> a) -> Identity a) -> (forall a. (Bool -> a) -> a)
    step_3 = (runIdentity .)

    step_4 :: (forall a. (Bool -> a) -> a) -> (forall a. (a, a) -> a)
    step_4 f (a, b) = f (\x' -> if x' then a else b)

    step_5 :: (forall a. (a, a) -> a) -> (forall a. a -> a -> a)
    step_5 = curry
