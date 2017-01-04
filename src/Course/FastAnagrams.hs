{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.

-- Playing with set
--
-- path :: Filename
-- path = listh "/Users/iurii_kyrylenko/Projects/haskell/wrk-practice/dict.txt"

-- t1 :: Filename -> IO (List Chars)
-- t1 = (<$>) (lines) . readFile -- t1 path

-- -- lookup dictionary
-- t2 :: Filename -> IO (S.Set Chars)
-- t2 = (<$>) (S.fromList . hlist . lines . map toLower) . readFile -- t2 path
-- -- t2 = (<$>) (S.fromList . hlist . lines) . readFile

-- -- lookup a group
-- t3 :: List Chars -> S.Set Chars -> List Chars
-- t3 ws s = filter (\w -> S.member (map toLower w) s) ws
-- -- t3 ws s = filter (\w -> S.member w s) ws

-- t4 :: Chars -> Filename -> IO (List Chars)
-- t4 w = (<$>) (t3 (permutations w) . S.fromList . hlist . lines) . readFile -- t4 "qwerty" path

-- -- >> t4 "qwerty123" path
-- -- []
-- -- (1.13 secs, 455,528,152 bytes)
-- --
-- -- >> anagrams "qwerty123" path
-- -- []
-- -- (3.24 secs, 1,838,829,568 bytes)

-- fastAnagrams  "QwertY" "/Users/iurii_kyrylenko/Projects/haskell/wrk-practice/dict.txt"
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams word =
  let lookup ws s = filter (\w -> S.member (map toLower w) s) ws
  in  (<$>) (lookup (permutations word) . S.fromList . hlist . lines . map toLower) . readFile
