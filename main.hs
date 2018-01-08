module Main where
import Parser
import ParsePrimitives

main :: IO ()
main = do
    contents <- getContents
    print $ parseGrammar contents
