{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Course.MonadTutorial where

import Control.Category(Category((.)))
import Control.Monad(Monad(..), (=<<))
import Data.Eq(Eq)
import Data.Foldable(foldr)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.String(IsString(fromString))
import Prelude(Show)
import System.IO(IO, getLine)

{-

--------------------------------------------------------------------------------
WARNING: DO NOT PROCEED
-----------------------

It is strongly advised that pre-requisite exercises have been covered prior to
utilising this tutorial. Refusing this advice increases likelihood of a crash
and burn result.

Please complete the following exercises before proceeding:
* Course/Functor
* Course/Applicative
--------------------------------------------------------------------------------

In this source file, you will find a recurring pattern:

* A data structure definition.
* A function named @bind<name>@ for that data structure. The bind function will
  follow a specific pattern in its type:

  @(a -> f b) -> f a -> f b@

* A function named @pure<name>@ for that data structure. The pure function will
  follow a specific pattern in its type:

  @a -> f a@

* A function named @sequence<name>@ for that data structure. The sequence
  function will follow a specific pattern in its type:

  @[f a] -> f [a]

Note that the sequence functions are written in terms of the bind and pure
functions for that data type. The goal is to first acknowledge the repeating
code in the sequence functions, and then construct a plan to refactor out the
similarities. Ultimately, there should be only a single sequence function that is
written in terms of "things that have bind and pure functions."

A type-class denoting "things that have bind and pure functions" is provided. It
is named @BindAndPure@.

Examine the existing data structures, their implementations of bind and pure,
then implement a single sequence function that generalises all the specific
sequence functions.

The data structures given are:
* Id
* Optional
* IntReader
* Reader
* IntState
* State
* Or
* ListFree
* IntReaderFree
* ReaderFree
* Free
* IO

-}

{-

-- Iurii Kyrylenko : desugaring 'do'

mySequence :: Monad m => [m a] -> m [a]
mySequence = foldr (\m ms -> do
  x <- m
  xs <- ms
  return (x:xs)
  ) (return [])

h m ms = do
  x <- m
  xs <- ms
  return (x:xs)

h m ms = m >>= (\x -> ms >>= (\xs -> return (x:xs)))

h m ms = m >>= \x -> ms >>= \xs -> return (x:xs)

h m ms =
  m >>= \x ->
  ms >>= \xs ->
  return (x:xs)

mySequence :: Monad m => [m a] -> m [a]
mySequence = foldr h (return [])

t1 = mySequence [Just 1, Just 2, Just 3]
t2 = mySequence [Just 1, Nothing, Just 3]
t3 = mySequence [[1,2,3], [4,5,6], [7,8,9]]

sequenceId :: [Id a] -> Id [a]
sequenceId =
  foldr (\m ms ->
    bindId (\x ->
    bindId (\xs ->
    pure (x:.xs)) ms) m
  ) (pureId Nil)
-}

data Id a =
  Id a
  deriving (Eq, Show)

instance BindAndPure Id where
  bind ::
    (a -> Id b)
    -> Id a
    -> Id b
  bind f (Id a) =
    f a

  pure ::
    a
    -> Id a
  pure =
    Id

----

data Optional a =
  Empty
  | Full a
  deriving (Eq, Show)

instance BindAndPure Optional where
  bind ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  bind _ Empty =
    Empty
  bind f (Full a) =
    f a

  pure ::
    a
    -> Optional a
  pure =
    Full

----

data IntReader a =
  IntReader (Int -> a)

-- read :: IntReader a -> Int -> a
-- read (IntReader f) i = f i
-- t = read (sequence [IntReader (\i -> 123), IntReader (\i -> 124)]) 42 -- [123, 124]

instance BindAndPure IntReader where
  bind ::
    (a -> IntReader b)
    -> IntReader a
    -> IntReader b
  bind f (IntReader g) =
    IntReader (\x -> let IntReader r = f (g x) in r x)

  pure ::
    a
    -> IntReader a
  pure =
    IntReader . return

----

data Reader r a =
  Reader (r -> a)

-- read :: Reader r a -> r -> a
-- read (Reader f) i = f i
-- t = read (sequence [Reader (\i -> 123), Reader (\i -> 124)]) 42 -- [123, 124]

instance BindAndPure (Reader r) where
  bind ::
    (a -> Reader r b)
    -> Reader r a
    -> Reader r b
  bind f (Reader g) =
    Reader (\x -> let Reader r = f (g x) in r x)

  pure ::
    a
    -> Reader r a
  pure =
    Reader . return

----

data IntState a =
  IntState (Int -> (a, Int))

instance BindAndPure IntState where
  bind ::
    (a -> IntState b)
    -> IntState a
    -> IntState b
  bind f (IntState g) =
    IntState (\i ->
      let (a, j) = g i
          IntState h = f a
      in h j)

  pure ::
    a
    -> IntState a
  pure a =
    IntState (\i -> (a, i))

----

data State s a =
  State (s -> (a, s))

instance BindAndPure (State s) where
  bind ::
    (a -> State s b)
    -> State s a
    -> State s b
  bind f (State g) =
    State (\s ->
      let (a, t) = g s
          State h = f a
      in h t)

  pure ::
    a
    -> State s a
  pure a =
    State (\s -> (a, s))

----

data Or t a =
  This t
  | That a
  deriving (Eq, Show)

-- t1 = sequence [This 42, That 43] -- This 42
-- t2 = sequence [That 42, This 43] -- This 43

instance BindAndPure (Or t) where
  bind ::
    (a -> Or t b)
    -> Or t a
    -> Or t b
  bind _ (This t) =
    This t
  bind f (That a) =
    f a

  pure ::
    a
    -> Or t a
  pure =
    That

----

data ListFree a =
  ListDone a
  | ListMore [ListFree a]
  deriving (Eq, Show)

instance BindAndPure ListFree where
  bind ::
    (a -> ListFree b)
    -> ListFree a
    -> ListFree b
  bind f (ListDone a) =
    f a
  bind f (ListMore r) =
    ListMore (fmap (bind f) r)

  pure ::
    a
    -> ListFree a
  pure =
    ListDone

----

data IntReaderFree a =
  IntReaderDone a
  | IntReaderMore [IntReaderFree a]
  deriving (Eq, Show)

instance BindAndPure IntReaderFree where
  bind ::
    (a -> IntReaderFree b)
    -> IntReaderFree a
    -> IntReaderFree b
  bind f (IntReaderDone a) =
    f a
  bind f (IntReaderMore r) =
    IntReaderMore (fmap (bind f) r)

  pure ::
    a
    -> IntReaderFree a
  pure =
    IntReaderDone

----

data ReaderFree r a =
  ReaderDone a
  | ReaderMore (Reader r (ReaderFree r a))

instance BindAndPure (ReaderFree r) where
  bind ::
    (a -> ReaderFree r b)
    -> ReaderFree r a
    -> ReaderFree r b
  bind f (ReaderDone a) =
    f a
  bind f (ReaderMore (Reader r)) =
    ReaderMore (Reader (bind f . r))

  pure ::
    a
    -> ReaderFree r a
  pure =
    ReaderDone

----

data Free f a =
  Done a
  | More (f (Free f a))

instance Functor f => BindAndPure (Free f) where
  bind ::
    Functor f =>
    (a -> Free f b)
    -> Free f a
    -> Free f b
  bind f (Done a) =
    f a
  bind f (More r) =
    More (fmap (bind f) r)

  pure ::
    a
    -> Free f a
  pure =
    Done

----

-- data IO = …

-- test = sequence [getLine, getLine]

instance BindAndPure IO where
  bind ::
    (a -> IO b)
    -> IO a
    -> IO b
  bind f o =
    f =<< o

  pure ::
    a
    -> IO a
  pure =
    return

----

class BindAndPure f where
  bind ::
    (a -> f b)
    -> f a
    -> f b
  pure ::
    a
    -> f a

sequence :: BindAndPure m => [m a] -> m [a]
sequence =
  foldr (\a as ->
    bind (\a' ->
    bind (\as' ->
    pure (a' : as')) as) a)
  (pure [])
