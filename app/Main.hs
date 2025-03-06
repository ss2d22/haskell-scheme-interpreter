module Main where 
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Fixed (E0)

{-
 - Data type for the parser to return
 - Atom - String naming the Atom
 - List - Stores a list of other lisp values
 - DottedList - an improper list where all
 - the values except the last value and the
 - last value is stored as a seperate entry
 - Number - numbers
 - String - strings
 - Bool - booleans
 -}
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
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
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

{-
 - An atom is a letter or symbol, followed by any number of letters, digits, or symbols
 - first we check if the first character is a letter or symbol using the <|> operator from parser
 - <|> operator tries the first parser if it doesnt consume any inputs and fails then we try the parser on the other side
 - rest checks if it's letter or digit or symbol
 - we construct a list with first and rest and store it in an atom variable
 - we then try and match the atom to the string literals for true and false and if they do not match
 - we return the atom as an Atom back up to the Parser
 -}
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

{-
 - we use many1 to match 1 or more digits and the returned the string is passed into the read function to convert it to a number
 - the number is then converted to our custom Number type defined in LispVal
 - we use . to take the resuly from the righjt function and pass it into the left function
 -
 - result of many1 digit is Parser String so our combined (Number . read) can't operate on it so we use
 - LiftM to operate on the value in the Monad
 -
 - we apply LiftM on (Number . read) and the result is fed into the returned function
 -}
-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- Ex 1 q1b
-- same as above but using the sequencing operator (this looks the coolest so i am keeping this uncommented)
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \digits -> return $ Number (read digits)

-- Ex 1 q1a
-- same as above two but using do-notation
-- parseNumber :: Parser LispVal
-- parseNumber = do
--   digits <- many1 digit
--   return $ Number (read digits)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

{-
 - takes an input string, get's rid of spaces and checks if teh first charcter matches
 -}
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

{-
 - skips spaces
 -}
spaces :: Parser ()
spaces = skipMany1 space


main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)