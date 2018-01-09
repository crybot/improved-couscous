module Parser where

import Control.Applicative ((<|>), empty)
import Control.Monad.State
import Data.Char
import List.Transformer
import ParsePrimitives

data Symbol
  = Terminal String
  | NonTerminal String
  deriving (Show)

type Expr = [Symbol]

data Production =
  Production Symbol
             [Expr]
  deriving (Show)

type Grammar = [Production]

nonTerminal :: Parser Symbol
nonTerminal =
  token $ enclosed (char '<') (char '>') (NonTerminal <$> identifier)

terminal :: Parser Symbol
terminal = token $ enclosed (char '\"') (char '\"') (Terminal <$> some literal)

symbol :: Parser Symbol
symbol = terminal <|> nonTerminal

expr :: Parser Expr
expr = some symbol

production :: Parser Production
production = do
  name <- nonTerminal
  string "::="
  x <- expr
  xs <-
    many
      (do string "|"
          expr)
  return $ Production name (x : xs)

grammar :: Parser Grammar
grammar = do
  p <- production
  ps <-
    many
      (do some newline
          production)
  many newline
  return (p : ps)

parseGrammar :: String -> Grammar
parseGrammar g =
  case parse grammar g of
    [] -> []
    ((v, ""):_) -> v
    _ -> error "parse error"
