module ParsePrimitives where

import Control.Applicative (Alternative, (<|>), empty, many, some)
import Control.Monad.State
import Data.Char
type Parser a = StateT String [] a

parse :: Parser a -> String -> [(a, String)]
parse = runStateT

item :: Parser Char
item = do
  s <- get
  case s of
    [] -> lift []
    (x:xs) -> do
      put xs
      return x

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  x <- item
  if p x
    then return x
    else empty

char :: Char -> Parser Char
char x = satisfies (x ==)

letter :: Parser Char
letter = satisfies isLetter

alphaNum :: Parser Char
alphaNum = satisfies isAlphaNum

identifier :: Parser String
identifier = do
  x <- letter
  xs <- many alphaNum
  return (x : xs)

literal :: Parser Char
literal = satisfies isPrint

except :: String -> Parser Char
except cs = satisfies (not . (`elem` cs))

space :: Parser ()
space = do
  many $ satisfies ((&&) <$> isSpace <*> (/= '\n'))
  return ()

newline :: Parser ()
newline = token $ char '\n' >>= const (return ())

enclosed :: Parser a -> Parser b -> Parser c -> Parser c
enclosed left right parser = do
  left
  v <- parser
  right
  return v

token :: Parser a -> Parser a
token = enclosed space space

string :: String -> Parser String
string [] = token $ return []
string (x:xs) =
  token $ do
    char x
    string xs
    return (x : xs)
