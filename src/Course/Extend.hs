{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Extend where

import Course.Core
import Course.Id
import Course.List
import Course.Optional
import Course.Functor

-- | All instances of the `Extend` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g. (f <<=) . (g <<=) ≅ (<<=) (f . (g <<=))`
class Functor f => Extend f where
  -- Pronounced, extend.
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

infixr 1 <<=

-- | Implement the @Extend@ instance for @Id@.
--
-- >>> id <<= Id 7
-- Id (Id 7)
instance Extend Id where
  (<<=) ::
    (Id a -> b)
    -> Id a
    -> Id b
  f <<= w = Id (f w)
  -- f <<= w = f <$> (Id w)
  -- (<<=) f = (<$>) (f . Id)
  -- (<<=) f w = (<$>) f (Id w)
  --  cojoin w = Id w

-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]

-- cj :: List a -> List (List a)
-- cj Nil = Nil
-- cj a@(_:.xs) = a :. cj xs

-- ex :: (List a -> b) -> List a -> List b
-- ex f xs = f <$> cj xs

-- ex' :: (List a -> b) -> List a -> List b
-- ex' _ Nil = Nil
-- ex' f a@(_:.xs) = f a :. ex' f xs

instance Extend List where
  (<<=) ::
    (List a -> b)
    -> List a
    -> List b
  _ <<= Nil       = Nil
  f <<= a@(_:.xs) = f a :. (f <<= xs)

-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty

instance Extend Optional where
  (<<=) ::
    (Optional a -> b)
    -> Optional a
    -> Optional b

  f <<= o = (f . Full) <$> o
  -- f <<= o = f <$> (Full <$> o)

-- | Duplicate the functor using extension.
--
-- >>> cojoin (Id 7)
-- Id (Id 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty
cojoin ::
  Extend f =>
  f a
  -> f (f a)
cojoin = (<<=) id
