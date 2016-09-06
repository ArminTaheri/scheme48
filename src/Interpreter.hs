module Interpreter (
      readSingleExpr 
    , readManyExpr
    , runSchemeParser
    , runSchemeEvalOnce
    , runSchemeRepl
    , runSchemeProgram
  ) where

import System.IO
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readOct, readHex)
import Data.Functor
import Data.Maybe
import Data.IORef

-- General
data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
                   params  :: [String]
                 , vararg  :: Maybe String
                 , body    :: [LispVal]
                 , closure :: Env
               }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

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
  Port _               -> "<IO port>"
  IOFunc _             -> "<IO primitive>"
  PrimitiveFunc _      -> "<primitive>"
  Func {
      params  = params 
    , vararg  = vararg
    , body    = body
    , closure = closure 
  }                    -> "(lambda (" ++ unwords params 
                                      ++ (case vararg of
                                            Nothing -> ""
                                            Just arg -> " . " ++ arg) 
                                      ++ ") ...)"

instance Show LispVal where show = showVal

-- Error Messages
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
parseExpr = try parseNumber 
         <|> parseAtom
         <|> parseNumber
         <|> parseString
         <|> parseQuoted
         <|> parseList

readWithParser :: Parser a -> String -> ThrowsError a 
readWithParser parser input = case parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val 

readSingleExpr :: String -> ThrowsError LispVal
readSingleExpr = readWithParser parseExpr

readManyExpr :: String -> ThrowsError [LispVal]
readManyExpr = readWithParser (endBy parseExpr spaces)

runSchemeParser :: String -> IO ()
runSchemeParser = print . resolveValue . readSingleExpr

-- Evaluator
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env expr = case expr of
  Atom var                             -> getVar env var
  val@(Number _)                       -> return val
  val@(String _)                       -> return val
  val@(Bool _)                         -> return val
  DottedList [] _                      -> return $ List []
  List [Atom "quote", val]             -> return val
  List [Atom "if", pred, conseq, alt]  -> do
    evaledPred <- eval env pred
    case evaledPred of
      Bool False -> eval env alt
      _          -> eval env conseq
  List [Atom "set!", Atom var, form]   -> eval env form >>= setVar env var
  List [Atom "define", Atom var, form] -> eval env form >>= defineVar env var
  List (Atom "define" : List (Atom var : params) : body)
                                       -> do
    func <- makeNormalFunc env params body
    defineVar env var func
  List (Atom "define" : DottedList (Atom var : params) vararg : body)
                                       -> do
    func <- makeVarArgFunc vararg env params body
    defineVar env var func
  List (Atom "lambda" : List params : body)
                                       -> makeNormalFunc env params body
  List (Atom "lambda" : DottedList params vararg : body)
                                       -> makeVarArgFunc vararg env params body
  List (Atom "lambda" : vararg@(Atom _) : body)
                                       -> makeVarArgFunc vararg env [] body
  List [Atom "load", String filename]  -> do
    exprs <- load filename 
    last <$> mapM (eval env) exprs
  List (function : args)               -> do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
  _                                    -> unrecognized
  where
    unrecognized = throwError $ BadSpecialForm "Unrecognized special form" expr

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply func args = case func of
  IOFunc func        -> func args
  PrimitiveFunc func -> liftThrows $ func args 
  Func {
      params  = params
    , vararg  = vararg
    , body    = body
    , closure = closure
  }                  -> if length params /= length args && isNothing vararg
                          then throwError $ NumArgs (toInteger . length $ params) args
                          else do
                            paramEnv <- liftIO . bindVars closure $ zip params args
                            extendedEnv <- case vararg of
                              Just arg -> let tailArgs = List $ drop (length params) args in
                                liftIO $ bindVars paramEnv [(arg, tailArgs)]
                              Nothing  -> return paramEnv
                            last <$> mapM (eval extendedEnv) body

-- Lisp Primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
               ("+", numericFoldingBinop 0 (+))
             , ("-", numericFoldingBinop 0 (-))
             , ("*", numericFoldingBinop 1 (*))
             , ("/", numericFoldingBinop 1 div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("=", numericFoldingBoolBinop (==))
             , ("<", numericFoldingBoolBinop (<))
             , (">", numericFoldingBoolBinop (>))
             , (">=", numericFoldingBoolBinop (>=))
             , ("<=", numericFoldingBoolBinop (<=))
             , ("&&", fmap (Bool . and) . mapM extractBool)
             , ("||", fmap (Bool . or ) . mapM extractBool)
             , ("not", boolUnary not)
             , ("string=?", stringFoldingBoolBinop (==))
             , ("string<?", stringFoldingBoolBinop (<))
             , ("string>?", stringFoldingBoolBinop (>))
             , ("string<=?", stringFoldingBoolBinop (<=))
             , ("string>=?", stringFoldingBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("eqv", eqv)
             , ("equal?", equal)
             ]

extractNum :: LispVal -> ThrowsError Integer
extractNum n = case n of
  Number n -> return n
  _        -> throwError $ TypeMismatch "number" n

extractBool :: LispVal -> ThrowsError Bool 
extractBool b = case b of
  Bool b -> return b
  _      -> throwError $ TypeMismatch "boolean" b

extractStr :: LispVal -> ThrowsError String 
extractStr s = case s of
  String s -> return s
  _      -> throwError $ TypeMismatch "string" s

lispBinop
  :: (a -> LispVal)
  -> (LispVal -> ThrowsError a)
  -> (a -> a -> a)
  -> [LispVal]
  -> ThrowsError LispVal
lispBinop constructor extractor op args = case args of
  [_, _] -> constructor . foldl1 op <$> mapM extractor args
  _      -> throwError $ NumArgs 2 args

lispFoldingBinop
  :: (a -> LispVal)
  -> (LispVal -> ThrowsError a)
  -> a
  -> (a -> a -> a)
  -> [LispVal]
  -> ThrowsError LispVal
lispFoldingBinop constructor extractor identity op args = case args of
  []     -> throwError $ NumArgs 1 []
  [_]    -> lispFoldingBinop constructor extractor identity op (constructor identity:args)
  params -> constructor . foldl1 op <$> mapM extractor params

lispFoldingBoolBinop
  :: (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
lispFoldingBoolBinop extractor op args = case args of
  []     -> throwError $ NumArgs 2 []
  [_]    -> throwError $ NumArgs 2 args
  params -> maybeToBool . foldBoolBinop op <$> mapM extractor params
  where
    foldBoolBinop op = foldl1 (maybeOp op) . map Just
    maybeOp op left right = case (left, right) of
      (Just left, Just right) -> if op left right then Just right else Nothing
      (Nothing, _)            -> Nothing
    maybeToBool maybe = case maybe of
      Just _  -> Bool True
      Nothing -> Bool False

boolUnary :: (Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolUnary op args = case args of
  [x] -> fmap (Bool. op) . extractBool $ x
  _ -> throwError $ NumArgs 1 args

numericBinop
  :: (Integer -> Integer -> Integer)
  -> [LispVal]
  -> ThrowsError LispVal
numericBinop = lispBinop Number extractNum

numericFoldingBinop
  :: Integer
  -> (Integer -> Integer -> Integer)
  -> [LispVal]
  -> ThrowsError LispVal
numericFoldingBinop = lispFoldingBinop Number extractNum

numericFoldingBoolBinop
  :: (Integer -> Integer -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
numericFoldingBoolBinop = lispFoldingBoolBinop extractNum

stringFoldingBoolBinop
  :: (String -> String -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
stringFoldingBoolBinop = lispFoldingBoolBinop extractStr 

car :: [LispVal] -> ThrowsError LispVal
car args = case args of
  [List (x : xs)]         -> return x
  [DottedList (x : xs) _] -> return x
  [nonPair]               -> throwError $ TypeMismatch "pair" nonPair
  _                       -> throwError $ NumArgs 1 args

cdr :: [LispVal] -> ThrowsError LispVal
cdr args = case args of
  [List (x : xs)]         -> return $ List xs
  [DottedList [_] x]      -> return x
  [DottedList (_ : xs) x] -> return $ DottedList xs x
  [nonPair]               -> throwError $ TypeMismatch "pair" nonPair
  _                       -> throwError $ NumArgs 1 args

cons :: [LispVal] -> ThrowsError LispVal
cons args = case args of
  [x, List xs]            -> return $ List (x : xs)
  [x, DottedList xs last] -> return $ DottedList (x : xs) last
  [x1, x2]                -> return $ DottedList [x1] x2
  _                       -> throwError $ NumArgs 2 args
eqv :: [LispVal] -> ThrowsError LispVal
eqv args = case args of
  [Atom x, Atom y]                            -> return . Bool $ x == y
  [Number x, Number y]                        -> return . Bool $ x == y
  [String x, String y]                        -> return . Bool $ x == y
  [Bool x, Bool y]                            -> return . Bool $ x == y
  [List xs, List ys]                          -> do
    pairEqvs <- mapM eqv [[x,y] | (x,y) <- zip xs ys]
    equivalences <- mapM extractBool pairEqvs
    return . Bool . and $ equivalences
  [DottedList xs xlast, DottedList ys ylast]  -> eqv [List (xlast:xs), List (ylast:ys)]
  [_, _]                                      -> return $ Bool False
  _                                           -> throwError $ NumArgs 2 args

data Projection = forall a. Eq a => Projection (LispVal -> Maybe a)

numProject :: LispVal -> Maybe Integer
numProject n = case n of
  Number n -> Just n
  Bool n   -> Just $ if n then 1 else 0
  String n -> let parsed = reads n :: [(Integer, String)]
              in if null parsed then Nothing else Just $ fst . head $ parsed
  _        -> Nothing

strProject :: LispVal -> Maybe String
strProject s = case s of
  Number s -> Just $ show s
  Bool s   -> Just $ show s
  String s -> Just s
  _        -> Nothing

boolProject :: LispVal -> Maybe Bool
boolProject b = case b of
  Number b -> Just $ b /= 0
  Bool b   -> Just b
  String b -> Just $ b == "#t"
  _        -> Nothing

equalUnder :: Projection -> LispVal -> LispVal -> Maybe Bool
equalUnder (Projection proj) arg1 arg2 = do
  projArg1 <- proj arg1
  projArg2 <- proj arg2
  let equals = return $ projArg1 == projArg2
  return $ fromMaybe False equals

equal :: [LispVal] -> ThrowsError LispVal
equal args = case args of
  [arg1, arg2] -> do
    isEqv <- eqv args
    let projEq = any (fromMaybe False) $ applicativeProj <*> [arg1] <*> [arg2]
    let (Bool equiv) = isEqv
    return . Bool $ equiv || projEq
  _            -> throwError $ NumArgs 2 args
  where
    projections = [Projection numProject, Projection strProject, Projection boolProject]
    applicativeProj = map equalUnder projections

runSchemeEval :: Env -> String -> IO ()
runSchemeEval env expr = do
  let evaled = show <$> (liftThrows (readSingleExpr expr) >>= eval env)
  resolved <- runIOThrows . trapError $ evaled
  putStrLn resolved

-- IO Primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
                 ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc args = case args of
  [func, List args] -> apply func args
  (func : args)     -> apply func args
  _                 -> throwError . Default $ "invalid arguments" ++ show args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode args = case args of
  [String filename] -> liftIO $ Port <$> openFile filename mode
  _                 -> throwError . Default $ "invalid arguments" ++ show args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort args = case args of
  [Port port] -> liftIO $ hClose port >> return (Bool True)
  _           -> return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc args = case args of
  []          -> readProc [Port stdin]
  [Port port] -> liftIO (hGetLine port) >>= liftThrows . readSingleExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc args = case args of
  [obj] -> writeProc [obj, Port stdout]
  [obj, Port port] -> liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents args = case args of
  [String filename] -> String <$> liftIO (readFile filename)
  _                 -> throwError . Default $ "invalid arguments" ++ show args

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readManyExpr

readAll :: [LispVal] -> IOThrowsError LispVal
readAll args = case args of
  [String filename] -> List <$> load filename
  _                 -> throwError . Default $ "invalid arguments" ++ show args

-- REPL
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $
    action result >> until_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
  where
    flushStr str = putStr str >> hFlush stdout

runSchemeRepl :: IO ()
runSchemeRepl = primitiveEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . runSchemeEval

runSchemeEvalOnce :: String -> IO ()
runSchemeEvalOnce expr = primitiveEnv >>= flip runSchemeEval expr

runSchemeProgram :: [String] -> IO ()
runSchemeProgram args = unless (null args) $ do
  env <- primitiveEnv >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  out <- runIOThrows $ show <$> eval env (List [Atom "load", String (head args)])
  hPutStrLn stderr out

-- Program State
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

primitiveEnv :: IO Env
primitiveEnv = nullEnv >>= flip bindVars funcs 
  where
    funcs = map (makeFunc PrimitiveFunc) primitives 
            ++ map (makeFunc IOFunc) ioPrimitives
    makeFunc constructor (var, func) = (var, constructor func)

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows thrower = case thrower of
  Left err -> throwError err
  Right val -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = resolveValue <$> runErrorT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError unbound) (liftIO . readIORef) (lookup var env)
  where
    unbound = UnboundVar "Unbound variable" var

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError unbound) (liftIO . flip writeIORef value) (lookup var env)
  return value
  where
    unbound = UnboundVar "Unbound variable" var

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  defined <- liftIO $ isBound envRef var
  if defined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef 
  extendedEnv <- (++ env) <$> mapM newRefBinding bindings 
  newIORef extendedEnv
  where
    newRefBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc vararg env params body = return $ Func (map showVal params) vararg body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgFunc :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgFunc = makeFunc . Just . showVal 

