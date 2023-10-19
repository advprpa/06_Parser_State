import Control.Applicative
import Data.Char (isDigit, isAlphaNum, isSpace, toUpper)
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
-- Basic Definitions
-------------------------------------------------------------------------------

newtype Parser a = P { parse :: String -> Maybe (a, String) }

-- item parses any Char
item :: Parser Char
item = P (\inp -> case inp of
                     (c:cs) -> Just (c, cs)
                     _      -> Nothing)


e1, e2 :: Maybe (Char, String)
e1 = parse item "abc"
e2 = parse item ""

-- sat parses a Char if it evaluates to True with the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = P (\inp -> case inp of
                     (c:cs) | p c -> Just (c, cs)
                     _            -> Nothing)

-- item, based on sat
item' :: Parser Char
item' = sat (\_ -> True)

-- digit only parses digits
digit :: Parser Char
digit = sat isDigit

-- alphaNum only parses alphanumeric characters (no spaces and control characters)
alphaNum :: Parser Char
alphaNum = sat isAlphaNum

-- char c parses a Char if it matches the given c
char :: Char -> Parser Char
char c = sat (== c)


e3, e4 :: Maybe (Char, String)
e3 = parse digit "1ab"
e4 = parse (char '<') "{as}"


-------------------------------------------------------------------------------
-- Sequencing Parsers
-------------------------------------------------------------------------------

-- Functor instance for Parser
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          Nothing      -> Nothing
                          Just (v,out) -> Just (g v, out))


e5, e6 :: Maybe (Char, String)
e5 = parse (fmap toUpper item) "abc"
e6 = parse (fmap toUpper item) ""


-- Applicative instance for Parser
instance Applicative Parser where
  pure :: a -> Parser a
  pure v = P (\inp -> Just (v,inp))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> pa = P (\inp -> case parse pg inp of
                           Nothing      -> Nothing
                           Just (g,out) -> parse (fmap g pa) out)


e7 :: Maybe (Integer, String)
e7 = parse (pure 1) "abc"

dropMiddle :: Parser (Char, Char)
dropMiddle = pure g <*> item <*> item <*> item
  where g x y z = (x,z)

e8 :: Maybe ((Char, Char), String)
e8 = parse dropMiddle "abcdef"


-- string parser
string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs

-- string based on sequenceA
string' :: String -> Parser String
string' = sequenceA . map char

-- string based on traverse
string'' :: String -> Parser String
string'' = traverse char

e9 :: Maybe (String, String)
e9 = parse (string "abc") "abcde"


-- nTimes: Applies a Parser n times in sequence
nTimes :: Int -> Parser a -> Parser [a]
nTimes n p = sequenceA (replicate n p)


e10 :: Maybe ([Char], String)
e10 = parse (nTimes 4 item) "abcdef"


-------------------------------------------------------------------------------
-- Making Choices
-------------------------------------------------------------------------------

-- Alternative instance for Parser
instance Alternative Parser where
  empty :: Parser a
  empty = P (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         Nothing      -> parse q inp
                         Just (v,out) -> Just (v,out))


e11, e12 :: Maybe (Char, String)
e11 = parse (item <|> pure 'X') "abc"

e12 = parse (char 'x' <|> char 'a') "abc"


e13, e14 :: Maybe ([Char], String)
e13 = parse (some digit) "123abc" -- some parses at least one

e14 = parse (many digit) "123abc" -- many parses zero or more


-------------------------------------------------------------------------------
-- Handling Spaces
-------------------------------------------------------------------------------

space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

token :: Parser a -> Parser a
token p = pure (\_ r _ -> r) <*> space <*> p <*> space

symbol :: String -> Parser String
symbol ss = token (string ss)

e15, e16 :: Maybe (Char, String)
e15 = parse (token (char 'X')) "   X   Y "


token' :: Parser a -> Parser a
token' p = space *> p <* space

e16 = parse (token' (char 'X')) "   X   Y "

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- Monad instance for Parser
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = P (\inp -> case parse pa inp of
                             Nothing -> Nothing
                             Just (a, rest) -> parse (f a) rest)

-- Sat based on item and 
sat' :: (Char -> Bool) -> Parser Char
sat' p = do
  c <- item
  if p c then pure c else empty



nat :: Parser Int
nat = fmap read (some digit)

intP :: Parser Int
intP = nat <|> (pure negate <* char '-' <*> nat)


formatP :: Parser [[Int]]
formatP = many $ do
  n <- intP 
  nTimes n intP

e = parse formatP "3 12 23 32 2 32 34 6 8 3 92 9 2 9"
  

ints :: Parser [Int]
ints = symbol "[" *> many (intP <* symbol ";") <* symbol "]"

-- main function for testing
main :: IO ()
main = putStrLn $ show $ parse ints "[1; -4; 6; 8;]"

