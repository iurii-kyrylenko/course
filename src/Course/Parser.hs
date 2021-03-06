{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseError =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  deriving Eq


instance Show ParseError where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character: ", show [c]]
  show Failed =
    "Parse failed"

data ParseResult a =
  ErrorResult ParseError
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show (ErrorResult e) =
    show e
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult (ErrorResult _) =
  True
isErrorResult (Result _ _) =
  False

data Parser a = P {
  parse :: Input -> ParseResult a
}

-- | Produces a parser that always fails with @UnexpectedChar@ using the given character.
unexpectedCharParser :: Char -> Parser a
-- to contribute
unexpectedCharParser = P . const . ErrorResult . UnexpectedChar
-- unexpectedCharParser c =
--   P (\_ -> ErrorResult (UnexpectedChar c))

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3

-- to contribute
valueParser :: a -> Parser a
valueParser = P . flip Result
-- valueParser x = P $ \i -> Result i x

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse failed "abc")
-- True

-- to contribute
failed :: Parser a
failed = P $ const (ErrorResult Failed)
-- failed = P (\i -> ErrorResult Failed)

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character :: Parser Char
character = P $ \i -> case i of
  Nil    -> ErrorResult UnexpectedEof
  x :. xs -> Result xs x

-- | Return a parser that maps any succeeding result with the given function.
--
-- >>> parse (mapParser succ character) "amz"
-- Result >mz< 'b'
--
-- >>> parse (mapParser (+10) (valueParser 7)) ""
-- Result >< 17
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f (P p) = P $ \i -> case p i of
  ErrorResult e -> ErrorResult e
  Result j a -> Result j (f a)

-- | This is @mapParser@ with the arguments flipped.
-- It might be more helpful to use this function if you prefer this argument order.
flmapParser ::
  Parser a
  -> (a -> b)
  -> Parser b
flmapParser =
  flip mapParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "")
-- True
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "x")
-- True
bindParser ::
  (a -> Parser b)
  -> Parser a
  -> Parser b
bindParser f (P p) = P $ \i -> case p i of
  ErrorResult e -> ErrorResult e
  Result j a -> parse (f a) j

-- | This is @bindParser@ with the arguments flipped.
-- It might be more helpful to use this function if you prefer this argument order.
flbindParser ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
flbindParser =
  flip bindParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@ or @flbindParser@.
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) ::
  Parser a
  -> Parser b
  -> Parser b
a >>> b = bindParser (const b) a
-- a >>> b = flbindParser a (const b)

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
P p ||| P q = P $ \i ->
  let res = p i
  in  if isErrorResult res then q i
      else res
-- P p ||| P q = P $ \i -> case p i of
--   Result j a -> Result j a
--   ErrorResult _ -> q i

infixl 3 |||

-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @list1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list character) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
list :: Parser a -> Parser (List a)
list = (||| valueParser Nil) . list1
-- list p = list1 p ||| valueParser Nil

-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
--
-- /Tip:/ Use @bindParser@, @list@ and @valueParser@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> valueParser 'v')) "")
-- True
list1 :: Parser a -> Parser (List a)
list1 p = bindParser (\c ->
  bindParser (\r ->
    valueParser (c :. r)
    ) (list p)
  ) p

-- My solution:
-- list1 p = bindParser (\c ->
--   P (\i ->
--     let Result j r = parse (list p) i
--     in  Result j (c :. r)
--     )
--   ) p

-- playing ...
-- list1 p = bindParser (\c -> valueParser (c:.Nil)) p
-- list1 p = bindParser (\c -> P (\i -> Result i (c:.Nil)) ) p

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@, @unexpectedCharParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = bindParser
              (\c -> if p c then valueParser c else unexpectedCharParser c)
              character

-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.

-- to contribute
is :: Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions.
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces zero or a positive integer but fails if
--
--   * The input is empty.
--
--   * The input does not produce a valid series of digits
--
-- /Tip:/ Use the @bindParser@, @valueParser@, @list1@, @read@ and @digit@
-- functions.
-- >>> parse natural "123"
-- Result >< 123
--
-- >>> parse natural "123ab"
-- Result >ab< 123
--
-- >>> isErrorResult (parse natural "abc")
-- True
--
-- >>> isErrorResult (parse natural "")
-- True
natural :: Parser Int
natural = bindParser (\ds ->
  case read ds of
    Full n -> valueParser n
    Empty  -> failed
  ) (list1 digit)

-- natural = bindParser (\ds ->
--   let Full n = read ds
--   in  valueParser n
--   ) (list1 digit)

--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isSpace@ functions.
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
spaces1 :: Parser Chars
spaces1 = list1 space

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isLower@ functions.
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isUpper@ functions.
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isAlpha@ functions.
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @valueParser@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True

sequenceParser :: List (Parser a) -> Parser (List a)
sequenceParser = foldRight (\p a ->
  bindParser (\c ->
    bindParser (\r ->
      valueParser (c :. r)
      ) a
    ) p
  ) (valueParser Nil)

-- implemented recursively, like list1 (see below)
--
-- sequenceParser :: List (Parser a) -> Parser (List a)
-- sequenceParser Nil = valueParser Nil
-- sequenceParser (p :. ps) = bindParser (\c ->
--   bindParser (\r ->
--     valueParser (c :. r)
--     ) (sequenceParser ps)
--   ) p

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True

-- to contribute  s
thisMany :: Int -> Parser a -> Parser (List a)
thisMany n = sequenceParser . replicate n
-- thisMany = (sequenceParser .) . replicate
-- thisMany n p = sequenceParser $ replicate n p

-- | Write a parser for Person.age.
--
-- /Age: positive integer/
--
-- /Tip:/ Equivalent to @natural@
-- Iurii: What about 0?
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True

-- to contribute
ageParser :: Parser Int
ageParser = bindParser (\c -> if c == 0 then failed else valueParser c) natural

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters/
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True

-- firstNameParser :: Parser Chars
-- firstNameParser = bindParser (\c ->
--   bindParser (\r ->
--     valueParser (c:.r)
--     ) (list lower)
--   ) upper

-- to contribute
firstNameParser :: Parser Chars
firstNameParser = bindParser
                  (valueParser . flatten)
                  $ sequenceParser (thisMany 1 upper :. list lower :. Nil)

-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True

-- surnameParser :: Parser Chars
-- surnameParser = flbindParser upper (\a ->
--   flbindParser (thisMany 5 lower) (\b ->
--     flbindParser (list lower) (\c ->
--       valueParser (a:.b++c))))

-- to contribute
surnameParser :: Parser Chars
surnameParser = bindParser (valueParser . flatten) $ sequenceParser
                (  thisMany 1 upper
                :. thisMany 5 lower
                :. list lower
                :. Nil
                )

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< 'y'
--
-- >>> parse smokerParser "nabc"
-- Result >abc< 'n'
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
smokerParser :: Parser Char
smokerParser = is 'y' ||| is 'n'

-- | Write part of a parser for Person#phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::  Parser Chars
phoneBodyParser = list $ digit ||| is '.' ||| is '-'

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True

-- phoneParser :: Parser Chars
-- phoneParser = flbindParser digit (\a ->
--   flbindParser phoneBodyParser (\b ->
--     flbindParser (is '#') (\_ ->
--       valueParser (a:.b))))

-- to contribute
phoneParser :: Parser Chars
phoneParser = bindParser (valueParser . flatten) $ sequenceParser
                (  thisMany 1 digit
                :. phoneBodyParser
                :. is '#' >>> valueParser Nil -- swallow result
                :. Nil
                )

-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @valueParser@,
--            @(>>>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
personParser :: Parser Person
personParser = flbindParser ageParser (\pAge ->
  spaces1 >>>
  flbindParser firstNameParser (\pFirstName ->
  spaces1 >>>
  flbindParser surnameParser (\pSurname ->
  spaces1 >>>
  flbindParser smokerParser (\pSmoker ->
  spaces1 >>>
  flbindParser phoneParser (\pPhone ->
  valueParser (Person pAge pFirstName pSurname pSmoker pPhone))))))

-- personParser :: Parser Person
-- personParser = flbindParser ageParser (\pAge ->
--   flbindParser spaces1 (\_ ->
--   flbindParser firstNameParser (\pFirstName ->
--   flbindParser spaces1 (\_ ->
--   flbindParser surnameParser (\pSurname ->
--   flbindParser spaces1 (\_ ->
--   flbindParser smokerParser (\pSmoker ->
--   flbindParser spaces1 (\_ ->
--   flbindParser phoneParser (\pPhone ->
--   valueParser (Person pAge pFirstName pSurname pSmoker pPhone))))))))))

-- Make sure all the tests pass!


-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.

-- >>> parse ((+1) <$> natural) "42"
-- Result >< 43

instance Functor Parser where
  (<$>) ::
    (a -> b)
    -> Parser a
    -> Parser b
  (<$>) = bindParser . (valueParser .)
  -- (<$>) f = bindParser $ valueParser . f
  -- f <$> p = bindParser (\c -> valueParser (f c)) p

-- | Write an Applicative functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
-- >>> parse (P (\i -> Result i (+1)) <*> natural) "42qw"
-- Result >qw< 43
--
instance Applicative Parser where
  pure ::
    a
    -> Parser a
  pure = valueParser
  (<*>) ::
    Parser (a -> b)
    -> Parser a
    -> Parser b
  (<*>) = flip (bindParser . flip (<$>))
  -- (<*>) fp dp = bindParser (<$> dp) fp
  -- fp <*> dp = bindParser (<$> dp) fp
  -- fp <*> dp = bindParser (\f -> f <$> dp) fp
  -- fp <*> dp = bindParser (\f ->
  --   bindParser (\d ->
  --     valueParser (f d)
  --     ) dp
  --   ) fp

-- | Write a Monad instance for a @Parser@.
instance Monad Parser where
  (=<<) ::
    (a -> Parser b)
    -> Parser a
    -> Parser b
  (=<<) = bindParser
  -- f =<< m = bindParser (\x -> f x) m
