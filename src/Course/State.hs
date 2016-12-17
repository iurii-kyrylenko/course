{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  f <$> State g = State $ \s ->
    let (a, s1) = g s
    in  (f a, s1)
  -- f <$> p = State $ \s ->
  --   let (a, s1) = runState p s
  --   in  (f a, s1)

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> runState (State (\s -> ((+3), s ++ "apple":.Nil)) <*> State (\s -> (7, s ++ "banana":.Nil))) Nil
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State ff <*> State fd = State $ \s ->
    let (f, s1) = ff s
        (v, s2) = fd s1
    in  (f v, s2)
  -- pf <*> pd = State $ \s ->
  --   let (f, s1) = runState pf s
  --       (v, s2) = runState pd s1
  --   in  (f v, s2)

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  f =<< State fm = State $ \s ->
    let (a, s1) = fm s
    in  runState (f a) s1
  -- f =<< m = State $ \s ->
  --   let (a, s1) = runState m s
  --   in  runState (f a) s1

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec :: State s a -> s -> s
exec (State fp) = snd . fp
-- exec (State fp) s = snd $ fp s
-- exec p s = snd $ runState p s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval (State fp) = fst . fp
-- eval p s = fst $ runState p s

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State $ \s -> (s, s)

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put = State . const . (,) ()
-- put s = State $ const ((), s)

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)

--
-- Playing with state:
--
-- p :: Char -> State Int Bool
-- p x = get >>= (\s -> put (1 + s) >>= (const $ pure (x == 'c')))
--
-- t1 = runState (p 'c') 0
-- t2 = runState (p 'a' >>= \a -> p 'c' >>= \c -> return (a:.c:.Nil)) 0
-- t3 = runState (p 'a' >>= \a -> p 'c' >>= \c -> return ('a':.'c':.Nil)) 0
-- t4 x = runState (p x >>= \b -> if b then return $ Full x else return Empty) 0
--
-- p' x = p x >>= \b -> if b then return $ Full x else return Empty
-- t5 = runState (p' 'a' >>= \_ -> p' 'c' >>= \_ -> p' 'd') 0
--
-- -- Solution
-- t6 p xs = foldRight (\x a -> p x >>= \b -> if b then return $ Full x else a) (return Empty) xs
--

findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM p = foldRight
  (\x a ->
    p x >>= \b ->
    if b then return $ Full x else a)
  (return Empty)

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3

-- Playing with state
-- p :: Ord a => a -> State (S.Set a) Bool
-- p x = State $ \s -> if S.member x s then (True, s) else (False, S.insert x s)
--
-- t1 = runState (p 42) S.empty
-- t2 = runState (p 42) $ S.fromList [43, 42]
-- t3 = runState (findM p $ listh "qwe3rty12345") S.empty
-- t4 = runState (findM p $ listh "qwe6rty12345") S.empty
--
-- Solution
-- myFirstRepeat :: Ord a => List a -> Optional a
-- myFirstRepeat xs = eval (findM p xs) S.empty
--
-- t5 = myFirstRepeat $ listh "qwerty123asdfg1mnk"

firstRepeat :: Ord a => List a -> Optional a
firstRepeat xs =
  let p x = State $ \s -> (S.member x s, S.insert x s)
  in  eval (findM p xs) S.empty

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)

distinct :: Ord a => List a -> List a
distinct xs = eval (filtering p xs) S.empty
  where p x = State $ \s -> (S.notMember x s, S.insert x s)

-- Refactoring using high-order function listWithState
-- Also
-- (lift2 f g1 h1) x = f (g1 x) (h1 x)
-- (lift2 (lift2 f) g2 h2) x y = f (g2 x y) (h2 x y)
-- (lift2 (lift2 (lift2 f) g3 h3)) x y = f (g3 x y z) (h3 x y z)

listWithState :: Ord a =>
  ((a -> State (S.Set a) Bool) -> List a -> State (S.Set a) b) ->
  (a -> S.Set a -> Bool) ->
  List a ->
  b
listWithState g k xs =
  let p = State . lift2 (lift2 (,)) k S.insert
  in  eval (g p xs) S.empty

-- listWithState g k xs =
  -- let p x = State $ \s -> (lift2 (lift2 (,)) k S.insert) x s
  -- in  eval (g p xs) S.empty

-- listWithState g k xs =
--   let p x = State $ \s -> (k x s, S.insert x s)
--   in  eval (g p xs) S.empty

firstRepeat2:: Ord a => List a -> Optional a
firstRepeat2 = listWithState findM S.member

distinct2 :: Ord a => List a -> List a
distinct2 = listWithState filtering S.notMember

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True

-- toDigits :: Integer -> List Integer
-- toDigits n
--   | n < 0     = toDigits $ negate n
--   | n < 10    = n :. Nil
--   | otherwise = let (d, m) = divMod n 10
--                 in  m :. toDigits d
-- happyEval :: Integer -> Integer
-- happyEval = foldRight (\x a -> x*x + a) 0 . toDigits

-- happyCheck :: Integer -> State (S.Set Integer) Bool
-- happyCheck n =
--   let e = happyEval n
--       isOne = e == 1
--   in  State $ \s -> if isOne || S.member e s then (isOne, s) else runState (happyCheck e) (S.insert e s)

-- isHappy :: Integer -> Bool
-- isHappy n = eval (happyCheck n) S.empty

isHappy :: Integer -> Bool
isHappy x =
  let toDigits n
        | n < 0     = toDigits $ negate n
        | n < 10    = n :. Nil
        | otherwise = let (d, m) = divMod n 10
                      in  m :. toDigits d
      happyEval = foldRight (\n a -> n * n + a) 0 . toDigits
      happyCheck n =
        let e = happyEval n
            isOne = e == 1
        in  State $ \s -> if isOne || S.member e s then (isOne, s) else runState (happyCheck e) (S.insert e s)
  in  eval (happyCheck x) S.empty

-- t1 = sequence $ map (\n -> P.print (n, isHappy n)) (listh [1..100])
-- 1 7 10 13 19 23 28 31

isHappyOneLiner :: Integer -> Bool
isHappyOneLiner =
  contains 1 .
  firstRepeat .
  produce (
    toInteger . sum . map (join (*) . digitToInt) . show'
  )
