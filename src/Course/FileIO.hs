{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = getArgs >>= \args ->
  case args of
    (fname:.Nil) -> run fname
    _            -> putStrLn "usage ..."
-- main = getArgs >>= (\(fname:._) -> run fname)

-- main = do
--   (fname:.rest) <- getArgs
--   putStrLn fname

type FilePath = Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run :: Chars -> IO ()
run fname =
  readFile fname >>=
  getFiles . lines >>=
  printFiles

-- run fname =
--   readFile fname >>= \content ->
--   getFiles (lines content) >>= \list ->
--   printFiles list

getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles = sequence . (<$>) getFile
-- getFiles = sequence . (getFile <$>)
-- getFiles px = sequence $ getFile <$> px

getFile :: FilePath -> IO (FilePath, Chars)
getFile = lift2 (<$>) (,) readFile -- would never guess
-- getFile fname = lift2 (,) (pure fname) (readFile fname)
-- getFile fname = readFile fname >>= \content -> return (fname, content)

printFiles :: List (FilePath, Chars) -> IO ()
printFiles = void . sequence . (<$>) (uncurry printFile)
-- printFiles = void . sequence . ((uncurry printFile) <$>)
-- printFiles = void . sequence . ((\(x, y) -> printFile x y) <$>)
-- printFiles xs = void $ sequence $ (\(x, y) -> printFile x y) <$> xs

printFile :: FilePath -> Chars -> IO ()
printFile fname content = putStrLn $ "============ " ++ fname ++ "\n" ++ content

