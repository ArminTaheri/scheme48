module Interpreter (
    readExpr,
    runSchemeParser,
    runSchemeEval
  ) where

import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment
import Numeric (readOct, readHex)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Pretty Print 
showVal :: LispVal -> String 
showVal expr = case expr of
  String contents      -> "\"" ++ contents ++ "\""
  Atom name            -> name
  Number contents      -> show contents
  Bool True            -> "#t"
  Bool False           -> "#f"
  List contents        -> "(" ++ unwordsList contents ++ ")"
  DottedList head tail -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
  where
    unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ noneOf "\\\"" <|> escape
  char '"'
  return $ String str
  where
    escape = do
      char '\\'
      c <- oneOf "\\\"0nrvtbf"
      return $ case c of
        '0' -> '\0'
        'n' -> '\n'
        'r' -> '\r'
        'v' -> '\v'
        't' -> '\t'
        'b' -> '\b'
        'f' -> '\f'
        x   -> x

parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = Number . read <$> many1 digit

parseRadixNumber :: Parser LispVal
parseRadixNumber = do
  char '#'
  parseBinary <|> parseOctal <|> parseDecimal <|> parseHex

parseBinary :: Parser LispVal
parseBinary = do
  char 'b'
  n <- many $ oneOf "01"
  return . Number . bin2int $ n

bin2int :: String -> Integer
bin2int = sum . zipWith (\d b -> b*2^d) [0..] . reverse . map p 
  where
    p '0' = 0
    p '1' = 1

parseOctal :: Parser LispVal
parseOctal = do
  char 'o'
  n <- many $ oneOf "01234567"
  return . Number . readWith readOct $ n

parseDecimal :: Parser LispVal
parseDecimal = do
  char 'd'
  n <- many1 digit 
  return . Number . read $ n

parseHex :: Parser LispVal
parseHex = do
  char 'x'
  n <- many $ oneOf "0123456789abcdefABCDEF"
  return . Number . readWith readHex $ n

readWith :: ReadS Integer -> String -> Integer
readWith reader = fst . head . reader

parseList :: Parser LispVal
parseList = do
  char '('
  parseInsideList []

parseInsideList :: [LispVal] -> Parser LispVal
parseInsideList expr = (spaces >> parseInsideList expr)
                    <|> parseEnd expr 
                    <|> parseDotted expr
                    <|> parseRest expr


parseDotted :: [LispVal] -> Parser LispVal
parseDotted expr = do 
  char '.'
  spaces
  tailExpr <- parseExpr
  char ')'
  return $ DottedList (reverse expr) tailExpr

parseRest :: [LispVal] -> Parser LispVal
parseRest expr = do
  next <- parseExpr
  parseInsideList (next:expr)

parseEnd :: [LispVal] -> Parser LispVal
parseEnd expr = do 
  char ')'
  return . List $ reverse expr

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseNumber
         <|> parseString
         <|> parseAtom
         <|> parseQuoted
         <|> parseList

readExpr :: String -> LispVal 
readExpr input = case parse parseExpr "lisp" input of
  Left err -> error $ "No match: " ++ show err
  Right val -> val 

runSchemeParser :: IO () 
runSchemeParser = getArgs >>= print . readExpr . head

-- Evaluator
eval :: LispVal -> LispVal
eval expr = case expr of
  List [Atom "quote", val] -> val
  _                        -> expr

runSchemeEval :: IO ()
runSchemeEval = getArgs >>= print . eval . readExpr . head

