module Parsers where
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.Char

type Parser a = StateT String (MaybeT Identity) a

parse :: Parser a -> String -> Maybe (a, String)
parse p s = runIdentity (runMaybeT (runStateT p s))

item :: Parser Char
item = do
  c:cs <- get
  put cs
  return c

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
    then return c
    else empty

digit :: Parser Char
digit = sat isDigit

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)





string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs

space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol ss = token (string ss)


