{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show)

-- Implement a Functor instance for Compose

-- >>> (+1) <$> (Compose ((Full 42) :. Nil))
-- compose [Full 43]

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
  k <$> Compose x = Compose $ (k <$>) <$> x

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where

-- Implement the pure function for an Applicative instance for Compose

-- >>> pure 42 :: Compose List Optional Int
-- >>> compose [Full 42]

  pure = Compose . pure . pure

-- Implement the (<*>) function for an Applicative instance for Compose

-- >>> Compose ((Full (+1)) :. Nil) <*> Compose ((Full 42) :. Nil)
-- >>> Compose [Full 43]

  Compose x <*> Compose y = Compose $ lift2 (<*>) x y

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
-- http://stackoverflow.com/questions/29453915/composing-monads-v-applicative-functors
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
