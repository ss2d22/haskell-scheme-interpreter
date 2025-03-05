module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

{-
 - Checks if  the passed in character matches one of the symbols allowed in Scheme identifiers
-}
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

{-
 - takes an input string, get's rid of spaces and checks if teh first charcter matches
-}
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "soton" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

{-
 - skips spaces
-}
spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
