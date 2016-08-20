module Interpreter (
    readExpr,
    runSchemeParser,
    runSchemeEval
  ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readOct, readHex)

-- General
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError err = case err of
  NumArgs expected found      -> "Expected " ++ show expected ++ " args; found " ++ show found
  TypeMismatch expected found -> "Invalid Type: expected " ++ expected ++ ", found " ++ show found
  Parser parseErr             -> "Parse error at " ++ show parseErr
  BadSpecialForm message form -> message ++ ": " ++ show form
  NotFunction message func    -> message ++ ": " ++ show func
  UnboundVar message varname  -> message ++ ": " ++ varname

instance Show LispError where show = showError

instance Error LispError where
  noMsg  = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

resolveValue :: ThrowsError a -> a
resolveValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
parsePlainNumber = do
  neg <- optionMaybe $ char '-'
  case neg of
    Nothing  -> parseAndReadNumber ""
    Just _   -> parseAndReadNumber "-"
  where
    parseAndReadNumber pre = Number . read . (pre ++) <$> many1 digit


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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

runSchemeParser :: IO ()
runSchemeParser = getArgs >>= print . resolveValue. readExpr . head

-- Evaluator
eval :: LispVal -> ThrowsError LispVal
eval expr = case expr of
  val@(Atom _)             -> return val
  val@(Number _)           -> return val
  val@(String _)           -> return val
  val@(Bool _)             -> return val
  List [Atom "quote", val] -> return val
  List (Atom func : args)  -> mapM eval args >>= apply func
  _                        -> throwError $ BadSpecialForm "Unrecognized special form" expr

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args)
  $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
              ,("-", numericBinop (-))
              ,("*", numericBinop (*))
              ,("/", numericBinop div)
              ,("mod", numericBinop mod)
              ,("quotient", numericBinop quot)
              ,("remainder", numericBinop rem) ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op args = case args of
  []            -> throwError $ NumArgs 2 []
  singleton@[_] -> throwError $ NumArgs 2 singleton
  params        -> Number . foldl1 op <$> mapM extractNum params

extractNum :: LispVal -> ThrowsError Integer
extractNum (Number n) = return n
extractNum notNum = throwError $ TypeMismatch "Cannot operate on non numeric: " notNum

runSchemeEval :: IO ()
runSchemeEval = do
  (expr:_) <- getArgs
  validated <- return $ do
    parsed <- readExpr expr
    trapError $ show <$> eval parsed
  putStrLn $ resolveValue validated
