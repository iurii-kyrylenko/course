{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: Chars -> Filename -> IO (List Chars)
anagrams cs =
  (<$>) (intersectBy equalIgnoringCase (permutations cs) . lines) . readFile

-- anagrams cs path =
--   (<$>) (intersectBy equalIgnoringCase (permutations cs) . lines) (readFile path)

-- anagrams cs path =
--   let prm = intersectBy equalIgnoringCase (permutations cs)
--   in  (<$>) (prm . lines) (readFile path)

-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
-- anagrams cs path =
--   let prm = intersectBy equalIgnoringCase (permutations cs)
--   in  prm . lines <$> readFile path

-- anagrams cs path =
--   let prm = intersectBy equalIgnoringCase (permutations cs)
--   in  prm <$> lines <$> readFile path

-- anagrams cs path = intersectBy equalIgnoringCase (permutations cs) <$> lines <$> readFile path
-- anagrams cs path =
--   let iod = lines <$> readFile path
--       prm = intersectBy equalIgnoringCase (permutations cs)
--   in  prm <$> iod

-- import Course.Applicative
-- import Course.Monad
--
-- anagrams cs path = do
--   content <- readFile path
--   return $ intersectBy equalIgnoringCase (permutations cs) (lines content)

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase = (==) `on` (toLower <$>)

-- (\xs ys -> (toLower <$>) xs == (toLower <$>) ys) ≅ (==) `on` (toLower <$>)

-- (\x y -> toLower x == toLower y) ≅ ((==) `on` toLower)

-- equalIgnoringCase xs ys = all id $ zipWith (\x y -> toLower x == toLower y) xs ys
