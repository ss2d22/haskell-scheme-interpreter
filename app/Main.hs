module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

{-
 - Data type for the parser to return
 - Atom - String naming the Atom
 - List - Stores a list of other Goku values
 - DottedList - an improper list where all 
 - the values except the last value and the 
 - last value is stored as a seperate entry
 - Number - numbers
 - String - strings
 - Bool - booleans
 -}
data GokuVal = Atom String
             | List [GokuVal]
             | DottedList [GokuVal] GokuVal
             | Number Integer
             | String String
             | Bool Bool


{-
 - Checks if  the passed in character matches one of the symbols allowed in Scheme identifiers
 -}
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

{-
 - Parses a string between double quotes ignoring any double quotes 
 - and stopping at the second double quote
 -}
parseString :: Parser GokuVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

{-
 -
 -}
parseAtom :: Parser GokuVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest 
              return $ case atom of
                         "#t" -> Bool True 
                         "#f" -> Bool False 
                         _    -> Atom atom 


{-
 - takes an input string, get's rid of spaces and checks if teh first charcter matches
 -}
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "Goku" input of
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
