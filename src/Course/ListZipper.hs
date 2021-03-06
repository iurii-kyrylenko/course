{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]

data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

lefts ::
  ListZipper a
  -> List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a
  -> List a
rights (ListZipper _ _ r) =
  r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  f <$> (ListZipper l x r) = ListZipper (f <$> l) (f x) (f <$> r)

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  f <$> IsZ z = IsZ (f <$> z)
  _ <$> IsNotZ = IsNotZ

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> xs == toListZ (fromList xs)

fromList :: List a -> MaybeListZipper a
fromList Nil     = IsNotZ
fromList (x:.xs) = IsZ (ListZipper Nil x xs )

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> toOptional (fromOptional z) == z
toOptional :: MaybeListZipper a -> Optional (ListZipper a)
toOptional IsNotZ = Empty
toOptional (IsZ z)  = Full z

zipper ::
  [a]
  -> a
  -> [a]
  -> ListZipper a
zipper l x r =
  ListZipper (listh l) x (listh r)

fromOptional ::
  Optional (ListZipper a)
  -> MaybeListZipper a
fromOptional Empty =
  IsNotZ
fromOptional (Full z) =
  IsZ z

asZipper ::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asZipper f =
  asMaybeZipper (IsZ . f)

(>$>)::
  (ListZipper a -> ListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(>$>) =
  asZipper

asMaybeZipper ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
asMaybeZipper _ IsNotZ =
  IsNotZ
asMaybeZipper f (IsZ z) =
  f z

(-<<) ::
  (ListZipper a -> MaybeListZipper a)
  -> MaybeListZipper a
  -> MaybeListZipper a
(-<<) =
  asMaybeZipper

-- | Convert the given zipper back to a list.
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList :: ListZipper a -> List a
toList (ListZipper xs a ys) = reverse xs ++ a :. ys

-- | Convert the given (maybe) zipper back to a list.
toListZ ::
  MaybeListZipper a
  -> List a
toListZ IsNotZ =
  Nil
toListZ (IsZ z) =
  toList z

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus :: (a -> a) -> ListZipper a -> ListZipper a
withFocus f (ListZipper l x r) = ListZipper l (f x) r

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus :: a -> ListZipper a -> ListZipper a
setFocus = withFocus . const

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) ::
  ListZipper a
  -> a
  -> ListZipper a
(.=) =
  flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft :: ListZipper a -> Bool
hasLeft = not . isEmpty . lefts

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight :: ListZipper a -> Bool
hasRight = not . isEmpty . rights

-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> findLeft (const p) -<< fromList xs == IsNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- to contribute
-- >>> findLeft (== 42) (zipper [1,2,3,42,5,6,7,8,9,10] 777 [11,12])
-- [5,6,7,8,9,10] >42< [3,2,1,777,11,12]

findLeft :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findLeft p (ListZipper l x r) = case break p l of
  (_, Nil)    -> IsNotZ
  (xs, y:.ys) -> IsZ (ListZipper ys y (reverse xs ++ x :. r))
    
-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> findRight (const False) -<< fromList xs == IsNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
--
-- to contribute
-- >>> findRight (== 42) (zipper [11,12] 777 [1,2,3,42,5,6,7,8,9,10])
-- [3,2,1,777,11,12] >42< [5,6,7,8,9,10]

findRight :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findRight p (ListZipper l x r) = case break p r of
  (_, Nil) -> IsNotZ
  (xs, y:.ys) -> IsZ (ListZipper (reverse xs ++ x :. l) y ys)

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop :: ListZipper a -> ListZipper a
moveLeftLoop (ListZipper Nil x rs) = let (h :. t) = reverse (x :. rs) in ListZipper t h Nil
moveLeftLoop (ListZipper (l :. ls) x rs) = ListZipper ls l (x :. rs)

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop :: ListZipper a -> ListZipper a
moveRightLoop (ListZipper ls x Nil) = let (h :. t) = reverse (x :. ls) in ListZipper Nil h t
moveRightLoop (ListZipper ls x (r :. rs)) = ListZipper (x :. ls) r rs

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft :: ListZipper a -> MaybeListZipper a
moveLeft (ListZipper Nil _ _) = IsNotZ
moveLeft (ListZipper (l :. ls) x rs) = IsZ (ListZipper ls l (x :. rs))

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight :: ListZipper a -> MaybeListZipper a
moveRight (ListZipper _ _ Nil) = IsNotZ
moveRight (ListZipper ls x (r :. rs)) = IsZ (ListZipper (x :. ls) r rs)

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft :: ListZipper a -> MaybeListZipper a
swapLeft (ListZipper Nil _ _) = IsNotZ
swapLeft (ListZipper (h:.t) x r) = IsZ (ListZipper (x:.t) h r)

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight :: ListZipper a -> MaybeListZipper a
swapRight (ListZipper _ _ Nil) = IsNotZ
swapRight (ListZipper l x (h:.t)) = IsZ (ListZipper l h (x:.t))

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> dropLefts (zipper l x r) == zipper [] x r
dropLefts :: ListZipper a -> ListZipper a
dropLefts (ListZipper _ x r) = ListZipper Nil x r

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> dropRights (zipper l x r) == zipper l x []
dropRights :: ListZipper a -> ListZipper a
dropRights (ListZipper l x _) = ListZipper l x Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN :: Int -> ListZipper a -> MaybeListZipper a
moveLeftN n
  | n < 0     = moveRightN $ negate n
  | n == 0    = IsZ
  | otherwise = (moveLeft -<<) . (moveLeftN $ pred n)

-- moveLeftN n x
--   | n < 0     = moveRightN (-n) x
--   | n == 0    = IsZ x
--   | otherwise = asMaybeZipper moveLeft $ moveLeftN (n - 1) x

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN :: Int -> ListZipper a -> MaybeListZipper a
moveRightN n
  | n < 0     = moveLeftN $ negate n
  | n == 0    = IsZ
  | otherwise = (moveRight -<<) . (moveRightN $ pred n)

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3

moveLeftN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveLeftN' n x
  | n < 0    = moveRightN' (negate n) x
  | otherwise =
      let moveLeftCount 0 z _ = Right z
          moveLeftCount k z c =
            case moveLeft z of
              IsNotZ -> Left c
              IsZ z' -> moveLeftCount (k - 1) z' (c + 1)
      in  moveLeftCount n x 0

-- moveLeftN' n x
--   | n < 0     = moveRightN' (negate n) x
--   | otherwise = case moveLeftN n x of
--       IsNotZ -> Left  (length $ lefts x)
--       IsZ z  -> Right z

-- moveLeftN' n x
--   | n < 0     = moveRightN' (negate n) x
--   | otherwise =
--     let len = length $ lefts x
--     in  if n > len
--       then Left len
--       else let (IsZ z) = moveLeftN n x in Right z

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3

moveRightN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveRightN' n x
  | n < 0    = moveLeftN' (negate n) x
  | otherwise =
      let moveRightCount 0 z _ = Right z
          moveRightCount k z c =
            case moveRight z of
              IsNotZ -> Left c
              IsZ z' -> moveRightCount (k - 1) z' (c + 1)
      in  moveRightCount n x 0

-- moveRightN' n x
--   | n < 0     = moveLeftN' (negate n) x
--   | otherwise = case moveRightN n x of
--       IsNotZ -> Left (length $ rights x)
--       IsZ z  -> Right z

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><

nth :: Int -> ListZipper a -> MaybeListZipper a
nth n x
  | n < 0 = IsNotZ
  | otherwise = case moveLeftN' n x of {- probe -}
      Right (ListZipper _ _ ls) -> moveLeftN (length ls) x
      Left k  -> moveRightN (n - k) x

-- nth :: Int -> ListZipper a -> MaybeListZipper a
-- nth n x
--   | n < 0 = IsNotZ
--   | otherwise =
--       let len = length $ lefts x {- current position -}
--       in  moveRightN (n - len) x

-- nth :: Int -> ListZipper a -> MaybeListZipper a
-- nth n x =
--   let (h:.xs)  = toList x
--       z = ListZipper Nil h xs
--   in  moveRightN n z

-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> optional True (\z' -> index z' == i) (toOptional (nth i z))
index :: ListZipper a -> Int
index = length . lefts

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> toList lz == toList (end lz)
--
-- prop> rights (end lz) == Nil

end :: ListZipper a -> ListZipper a
end x = case moveRight x of
  IsNotZ -> x
  IsZ z -> end z

-- end :: ListZipper a -> ListZipper a
-- end x = let h:.t = reverse $ toList x
--         in  ListZipper t h Nil

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> toList lz == toList (start lz)
--
-- prop> lefts (start lz) == Nil
start :: ListZipper a -> ListZipper a
start x = case moveLeft x of
  IsNotZ -> x
  IsZ z -> start z

-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft :: ListZipper a -> MaybeListZipper a
deletePullLeft (ListZipper Nil _ _) = IsNotZ
deletePullLeft (ListZipper (h:.t) _ rs) = IsZ (ListZipper t h rs)

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight :: ListZipper a -> MaybeListZipper a
deletePullRight (ListZipper _ _ Nil) = IsNotZ
deletePullRight (ListZipper ls _ (h:.t)) = IsZ(ListZipper ls h t)

-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft :: a -> ListZipper a -> ListZipper a
insertPushLeft x (ListZipper ls y rs) = ListZipper (y:.ls) x rs

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight :: a -> ListZipper a -> ListZipper a
insertPushRight x (ListZipper ls y rs) = ListZipper ls x (y:.rs)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> all . (==) <*> take n . lefts . pure
--
-- prop> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Applicative ListZipper where
-- /Tip:/ Use @List#repeat@.
  pure a = ListZipper (repeat a) a (repeat a)
-- /Tip:/ Use `zipWith`
  (ListZipper fl f fr) <*> (ListZipper dl d dr) =
    ListZipper (zipWith ($) fl dl) (f d) (zipWith ($) fr dr)
    -- ListZipper (zipWith g fl dl) (f d) (zipWith g fr dr)
    -- where g x y = x y
    -- where g = ($)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> let is (IsZ z) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> let is (IsZ z) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> IsNotZ <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsNotZ
-- ><
--
-- >>> IsNotZ <*> IsNotZ
-- ><
instance Applicative MaybeListZipper where
  pure = IsZ . pure

-- to contribute
  IsZ x <*> IsZ y = IsZ (x <*> y)
  _     <*> _     = IsNotZ

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]

-- cjz :: ListZipper a -> ListZipper (ListZipper a)
-- cjz a = ListZipper (f moveLeft a) a (f moveRight a)
--         where f move = unfoldr $ ((\y -> (y,y)) <$>) . toOptional . move

-- cjz a = ListZipper (f moveLeft a) a (f moveRight a)
--         where f move = unfoldr (\x -> (\y -> (y,y))<$>(toOptional (move x)))
-- cjz a = ListZipper (fl a) a (fr a)
--         where fl = unfoldr (\x -> (\y -> (y,y))<$>(toOptional (moveLeft x)))
--               fr = unfoldr (\x -> case moveRight x of
--                 IsNotZ -> Empty
--                 IsZ res -> Full (res, res))
-- cjz a = ListZipper (fl a) a (fr a)
--         where fl z = unfoldr (\x -> case moveLeft x of
--                 IsNotZ -> Empty
--                 IsZ res -> Full (res, res)) z
--               fr z = unfoldr (\x -> case moveRight x of
--                 IsNotZ -> Empty
--                 IsZ res -> Full (res, res)) z

instance Extend ListZipper where
  -- to contribute
  f <<= a = f <$> ListZipper (g moveLeft a) a (g moveRight a)
    where g move = unfoldr $ ((\x -> (x,x)) <$>) . toOptional . move

  -- f <<= a = ListZipper (g moveLeft a) (f a) (g moveRight a)
  --   where g move = unfoldr $ ((\x -> (f x,x)) <$>) . toOptional . move

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= IsNotZ
-- ><
--
-- >>> id <<= (IsZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]

-- cjmz :: MaybeListZipper a -> MaybeListZipper (MaybeListZipper a)
-- cjmz IsNotZ = IsNotZ
-- cjmz (IsZ a) = IsZ $ ListZipper (f moveLeft a) (IsZ a) (f moveRight a)
--         where f move = unfoldr $ ((\y -> (IsZ y,y)) <$>) . toOptional . move

instance Extend MaybeListZipper where
  _ <<= IsNotZ = IsNotZ
  f <<= IsZ a  = IsZ (f . IsZ <<= a)

  -- f <<= a = f <$> cjmz a

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure (ListZipper _ a _) = a
    -- error "todo: Course.ListZipper copure#instance ListZipper"

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty

instance Traversable ListZipper where
  -- to contribute
  traverse f (ListZipper l a r) = lift3 ListZipper (traverse f l) (f a) (traverse f r)

  -- double 'reverse'
  -- traverse f (ListZipper l x r) =
  --   (ListZipper . reverse) <$> traverse f (reverse l) <*> f x <*> traverse f r

-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id IsNotZ
-- ><
--
-- >>> traverse id (IsZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]

instance Traversable MaybeListZipper where
  traverse f (IsZ z) = IsZ <$> traverse f z
  traverse _ IsNotZ = pure IsNotZ

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"

-----------------------
-- CHAIN APPLICATION --
-----------------------

(-:):: MaybeListZipper a -> (ListZipper a -> ListZipper a) -> MaybeListZipper a
(-:) = flip asZipper

(>:) :: MaybeListZipper a -> (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a
(>:) = flip asMaybeZipper

testZipperChain :: MaybeListZipper Int
testZipperChain = IsZ (zipper [1..9] 10 [11..19])
     -: withFocus (+42)
     >: moveLeftN 3
     -: withFocus (+42)
     >: moveLeft
     >: deletePullLeft
     -: insertPushRight 42
     >: nth 12
