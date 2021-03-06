{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  f <$> StateT t = StateT $ \s -> (\(a, s1) -> (f a, s1)) <$> t s

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where

  pure :: a -> StateT s f a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) ::
   StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b

  StateT ff <*> StateT fd = StateT $ \s ->
    join ((\(f, s1) -> (\(d, s2) -> (f d, s2)) <$> fd s1) <$> ff s)

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where

  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b

  f =<< StateT p = StateT $ \s ->
    (\(a, s1) -> runStateT (f a) s1) =<< p s

  -- f =<< StateT p = StateT $ \s -> do
  --   (a, s1) <- p s
  --   runStateT (f a) s1

  -- f =<< StateT p = StateT $ \s ->
  --   join ((\(a, s1) -> runStateT (f a) s1) <$> p s)

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' p = StateT $ Id . p
-- state' p = StateT $ \s -> Id (p s)

-- state' p = StateT $ \s ->
--   let (a, s1) = p s
--   in  Id (a, s1)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)

runState' (StateT p) = runId . p

-- runState' (StateT p) s = runId (p s)

-- runId (Id t) = runId (p s) = t

-- runState' (StateT p) s =
--   let Id t = p s
--   in  t

-- runState' w s =
--   let Id t = runStateT w s
--   in  t

-- runState' w s =
--   let Id (a, s1) = runStateT w s
--   in  (a, s1)

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT p) = (<$>) snd . p
-- execT (StateT p) s = (<$>) snd (p s)
-- execT w s = (<$>) snd (runStateT w s)

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' (StateT p) = snd . runId . p
-- exec' (StateT p) s = snd (runId (p s))
-- exec' (StateT p) s =
--   -- runId (Id a) = a
--   let Id (a, s1) = p s
--   in  s1
-- exec' w s =
--   let Id (a, s1) = runStateT w s
--   in  s1
-- exec' w s =
--   let Id (a, s1) = runStateT w s
--   in  snd $ (a, s1)
-- exec' w = snd . runState' w
-- exec' w s = snd $ runState' w s

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT p) = (<$>) fst . p

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' (StateT p) = fst . runId . p
-- eval' w = fst . runState' w

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT $ \s -> pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT = StateT . const . pure . (,) ()
-- putT s = StateT (const (pure ((), s)))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a) =>  List a -> List a
distinct' xs = eval' (filtering p xs) S.empty
  where p x = state' $ \s -> (S.notMember x s, S.insert x s)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty

-- Playing with state ----------------
-- mp :: (Ord a) => a -> State' (S.Set a) Bool
-- mp x = state' $ \s -> (S.notMember x s, S.insert x s)

-- mpi :: (Ord a) => a -> StateT (S.Set a) Id Bool
-- mpi x = StateT $ \s -> pure (S.notMember x s, S.insert x s)

-- mpo :: (Ord a) => a -> StateT (S.Set a) Optional Bool
-- mpo x = StateT $ \s -> Full (S.notMember x s, S.insert x s)

-- mpo100 :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
-- mpo100 x = StateT $ \s -> if x > 100 then Empty else Full (S.notMember x s, S.insert x s)
--------------------------------------

distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs = evalT (filtering p xs) S.empty
  where p x = StateT $ \s ->
              if x > 100 then Empty
                         else Full (S.notMember x s, S.insert x s)

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> OptionalT m = OptionalT $ (<$>) f <$> m
  -- f <$> OptionalT m = OptionalT $ mapOptional f <$> m

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where

  pure = OptionalT . pure . pure

  OptionalT ff <*> OptionalT fd = OptionalT $ lift2 (<*>) ff fd
  -- OptionalT ff <*> OptionalT fd = OptionalT $ (<*>) <$> ff <*> fd

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  f =<< OptionalT m = OptionalT $ (\o -> case o of
    Empty -> pure Empty
    Full x -> runOptionalT (f x)
    ) =<< m

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> Logger ls x = Logger ls (f x)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil
  Logger ls1 f <*> Logger ls2 d = Logger (ls1 ++ ls2) (f d) 

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  f =<< Logger ls x =
    let Logger ls1 x1 = f x
    in  Logger (ls ++ ls1) x1

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 = Logger . (:. Nil)
-- log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty

-- Playing with state ----------------
-- Optional --
-- p1 :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
-- p1 x = StateT $ \s -> if x > 100 then Empty else Full (S.notMember x s, S.insert x s)

-- -- OptionalT over Id (runOptionalT $ ...) --
-- p2 :: (Ord a, Num a) => a -> StateT (S.Set a) (OptionalT Id) Bool
-- p2 x = StateT $ \s -> if x > 100 then OptionalT (Id Empty) else OptionalT (Id (Full (S.notMember x s, S.insert x s)))

-- -- OptionalT over Id (runOptionalT $ ...) --
-- p3 :: (Ord a, Num a) => a -> StateT (S.Set a) (OptionalT Id) Bool
-- p3 x = StateT $ \s -> OptionalT (Id (
--   if x > 100
--     then Empty
--     else Full (S.notMember x s, S.insert x s)))

-- -- OptionalT over Logger (runOptionalT $ ...) --
-- p4 :: (Ord a, Num a, Show a) => a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
-- p4 x = StateT $ \s -> OptionalT (Logger (show' x :. Nil) (
--   if x > 100
--     then Empty
--     else Full (S.notMember x s, S.insert x s)))

-- OptionalT over Logger (runOptionalT $ ...) --
-- p :: (Ord a, Num a, Show a, Integral a) => a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
-- p x = StateT $ \s -> OptionalT (
--   let ok = Full (S.notMember x s, S.insert x s)
--   in  if x > 100
--         then log1 ("aborting > 100: " ++ show' x) Empty
--         else if even x
--           then log1 ("even number: " ++ show' x) ok
--           else Logger Nil ok
--   )

-- test xs = runStateT (filtering p (listh xs)) S.empty
-- test2 xs = evalT (filtering p (listh xs)) S.empty
--------------------------------------

distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
-- before refactoring
distinctG xs = runOptionalT $ evalT (filtering p xs) S.empty
  where p x = StateT $ \s -> OptionalT (
              let ok = Full (S.notMember x s, S.insert x s)
              in  if x > 100 then log1 ("aborting > 100: " ++ show' x) Empty
                  else if even x then log1 ("even number: " ++ show' x) ok
                  else Logger Nil ok
              )
-- after refactoring
-- distinctG xs = runOptionalT $ evalT (filtering p xs) S.empty
--   where p x = StateT $ \s -> OptionalT (
--               if x > 100
--                 then log1 ("aborting > 100: " ++ show' x) Empty
--                 else (if even x
--                         then log1 ("even number: " ++ show' x)
--                         else pure) $ Full (S.notMember x s, S.insert x s)
--               )
