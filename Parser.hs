module Parser where

import Control.Applicative ((<|>), empty)
import Control.Monad.State
import Data.Char
import List.Transformer
import ParsePrimitives

data Symbol
  = Terminal String
  | NonTerminal String
  deriving (Show, Eq)

type Expr = [Symbol]

data Production =
  Production Symbol
             [Expr]
  deriving (Show, Eq)

type Grammar = [Production]

showSymbol :: Symbol -> String
showSymbol (Terminal x) = "\"" ++ x ++ "\""
showSymbol (NonTerminal x) = "<" ++ x ++ ">"

showExpr :: Expr -> String
showExpr = foldr (((++) . (++ " ")) . showSymbol) ""

showExprs :: [Expr] -> String
showExprs [] = ""
showExprs [x] = showExpr x
showExprs (x:xs) = showExpr x ++ " | " ++ showExprs xs

showProduction :: Production -> String
showProduction (Production s exps) = showSymbol s ++ " ::= " ++ showExprs exps

showGrammar :: Grammar -> String
showGrammar = unlines . map showProduction

nonTerminal :: Parser Symbol
nonTerminal =
  token $ enclosed (char '<') (char '>') (NonTerminal <$> identifier)

terminal :: Parser Symbol
terminal =
  token $ enclosed (char '\"') (char '\"') (Terminal <$> some (except ['\"']))

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

appendChar :: Char -> Symbol -> Symbol
appendChar c (Terminal x) = Terminal (x ++ [c])
appendChar c (NonTerminal x) = NonTerminal (x ++ [c])

startsWith :: Symbol -> Expr -> Bool
startsWith _ [] = False
startsWith s exp = s == head exp

isLeftRecursiveP :: Production -> Bool
isLeftRecursiveP (Production name exps) = any (startsWith name) exps

rightRecursiveP :: Production -> [Production]
rightRecursiveP (Production name exps) = map replace exps
  where
    replace exp
      | startsWith name exp = Production name' [tail exp ++ [name']]
      | otherwise = Production name [exp ++ [name']]
    name' = appendChar '\'' name

regularize :: Grammar -> Grammar
regularize [] = []
regularize (x:xs)
  | isLeftRecursiveP x = rightRecursiveP x ++ regularize xs
  | otherwise = x : regularize xs
