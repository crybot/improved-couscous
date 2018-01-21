module Main where

import ParsePrimitives
import Parser

main :: IO ()
main = do
  contents <- getContents
  let ps = parseGrammar contents
  putStrLn "BEFORE"
  putStrLn . showGrammar $ ps
  putStrLn "AFTER"
  (putStrLn . showGrammar . regularize) ps
