{-# LANGUAGE RankNTypes #-}

module Parametricity where

import M03

data One = One
data Id a = Id a
instance Maps Id where
  maps f (Id a) = Id (f a)

data Yoneda f a = Yoneda (forall b. (a -> b) -> f b)
runYoneda :: Yoneda f a -> (forall b. (a -> b) -> f b)
runYoneda (Yoneda x) = x
runId :: Id a -> a
runId (Id x) = x

lowerYoneda :: Yoneda f a -> f a
lowerYoneda y = runYoneda y id

liftYoneda :: Maps f => f a -> Yoneda f a
liftYoneda a = Yoneda (\f -> maps f a)

lowerYoneda' :: (forall b. (a -> b) -> f b) -> f a
lowerYoneda' f = lowerYoneda (Yoneda f)

liftYoneda' :: Maps f => f a -> (a -> b) -> f b
liftYoneda' = runYoneda . liftYoneda

----

-- theorem : (a -> a) = 1
id_one_forward :: (forall a. a -> a) -> One
id_one_forward z =
  let step1 :: (forall a. a -> a) -> (forall a. (One -> a) -> a)
      step1 f g = f (g One)
      step2 :: (forall a. (One -> a) -> a) -> (forall a. (One -> a) -> Id a)
      step2 f = Id . f
      step3 :: (forall a. (One -> a) -> Id a) -> Id One
      step3 = lowerYoneda'
      step4 :: Id One -> One
      step4 = runId
  in  step4 (step3 (step2 (step1 z)))

id_one_backward :: One -> (forall a. a -> a)
id_one_backward z =
  let step1 :: One -> Id One
      step1 = Id
      step2 :: Id One -> (forall a. (One -> a) -> Id a)
      step2 = liftYoneda'
      step3 :: (forall a. (One -> a) -> Id a) -> (forall a. (One -> a) -> a)
      step3 f = runId . f
      step4 :: (forall a. (One -> a) -> a) -> (forall a. a -> a)
      step4 f a = f (\One -> a)
  in  step4 (step3 (step2 (step1 z)))

{-
composition_one_forwards :: (forall a b c. (b -> c) -> (a -> b) -> a -> c) -> One
composition_one_forwards = error "todo"

composition_one_backwards :: One -> (forall a b c. (b -> c) -> (a -> b) -> a -> c)
composition_one_backwards = error "todo"
-}

