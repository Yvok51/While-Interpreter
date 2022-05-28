{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import           Data.Char                      ( isAlphaNum
                                                , isDigit
                                                , isLetter
                                                , isSpace
                                                )

import           Evaluation                     ( Com
                                                  ( Assignment
                                                  , IfThen
                                                  , Seq
                                                  , While
                                                  )
                                                , Expr
                                                  ( BoolBinaryOp
                                                  , BinaryOp
                                                  , Bool
                                                  , Rel
                                                  , Not
                                                  , Number
                                                  , Var
                                                  )
                                                , Identifier
                                                , Op (Add)
                                                , RelOp (Eq)
                                                , BoolOp (And)
                                                )

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f stavovyVypocet =
    State $ \s -> let (s', a) = runState stavovyVypocet s in (s', f a)

instance Monad (State s) where
  -- return :: a -> State s a
  return a = State $ \s -> (s, a)

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  stavovyVypocet >>= f = State $ \s ->
    let (s', a)         = runState stavovyVypocet s
        stavovyVypocet' = f a
    in  runState stavovyVypocet' s'


instance Applicative (State s) where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s ()
set s = State $ const (s, ())

newtype Parser a = Parser {runParser :: State String (Either ParseError a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ do
    result <- p
    case result of
      Left  err -> return $ Left err
      Right a   -> return $ Right $ f a

instance Monad Parser where
  return x = Parser $ do
    return $ Right x

  (>>=) (Parser p) f = Parser $ do
    result <- p
    case result of
      Right x   -> runParser (f x)
      Left  err -> return $ Left err

instance Applicative Parser where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

data ParseError = ParseError
  { errorExpected :: String
  , errorFound    :: String
  }
  deriving Eq

instance Show ParseError where
  show err =
    "expected: " <> errorExpected err <> ", but found: " <> errorFound err

parseError :: String -> String -> Parser a
parseError expected found = Parser $ return $ Left $ ParseError expected found

-- Parse an end-of-file
parseEof :: Parser ()
parseEof = Parser $ do
  input <- get
  case input of
    []      -> return $ Right ()
    (c : _) -> return $ Left $ ParseError "end of file" [c]

-- Parse any character
parseAny :: Parser Char
parseAny = Parser $ do
  input <- get
  case input of
    []       -> return $ Left (ParseError "any character" "end of file")
    (c : xs) -> do
      set xs
      return $ Right c

-- Combine two parsers together
-- try the first parser, if it doesn't work try the second and return result
(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ do
  backup   <- get
  resultP1 <- p1
  case resultP1 of
    Right a -> return $ Right a
    Left  _ -> do
      set backup
      p2

-- Get a description of what we are parsing and a predicate for what we want to parse
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description p = do
  c <- parseAny
  if p c then return c else parseError description [c]

-- Build a parser for a given character
char :: Char -> Parser Char
char c = satisfy [c] (== c)

-- Parse whitespace
space :: Parser Char
space = satisfy "space" isSpace

-- Build a parser for a digit
digit :: Parser Char
digit = satisfy "digit" isDigit

-- Try to parse with a given parser as many times as possible
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- Try to parse with given parser as many times as possible but at least once
many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first : rest)

-- Build parser for a given string
string :: String -> Parser String
string = mapM char

-- Build a parser for continous whitespace
spaces :: Parser String
spaces = many space

-- Build a parser for a given string and as much whitespace after it
symbol :: String -> Parser String
symbol s = do
  result <- string s
  _      <- spaces
  return result

-- Parse between what the right and left parsers parse
between :: Parser a -> Parser c -> Parser b -> Parser b
between left right p = do
  _      <- left
  result <- p
  _      <- right
  return result

betweenParenthesis :: Parser a -> Parser a
betweenParenthesis = between (symbol "(") (symbol ")")

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> return Nothing

-- Build a parser for an integer
parseInteger :: Parser Int
parseInteger = do
  minus  <- optional $ symbol "-"
  digits <- many1 digit
  _      <- spaces
  case minus of
    Nothing -> return $ read digits
    Just _  -> return $ -1 * read digits

-- combine a list of parsers together using `<|>`
choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch where noMatch = parseError desc "no match"

-- Build a parser for a boolean value
parseBool :: Parser Bool
parseBool = trueParser <|> falseParser
 where
  trueParser :: Parser Bool
  trueParser = do
    _ <- symbol "true"
    return True

  falseParser :: Parser Bool
  falseParser = do
    _ <- symbol "false"
    return False

-- Build a parser for identifiers - string of letters, numbers and underscores
parseIdentifier :: Parser Identifier
parseIdentifier = do
  firstChar <- satisfy "Beginning of an identifier"
                       (\c -> isLetter c || c == '_')
  result <- many $ satisfy "identifier" (\c -> isAlphaNum c || c == '_')
  _      <- spaces
  return $ firstChar : result

parseLeftAssoc :: Parser a -> Parser (a -> a -> a) -> Parser a
parseLeftAssoc p op = do
    lhs <- p
    process lhs
  where
    process lhs = do
      maybeOp <- optional op
      case maybeOp of 
        Nothing -> return lhs
        Just op -> do
          rhs <- p
          process $ op lhs rhs

parseRightAssoc :: Parser a -> Parser (a -> a -> a) -> Parser a
parseRightAssoc p op = do
    lhs <- p
    process lhs
  where
    process lhs = do
      maybeOp <- optional op
      case maybeOp of
        Nothing -> return lhs
        Just parsedOp -> do
          rhs <- parseRightAssoc p op
          return $ parsedOp lhs rhs

-- E ::= <integer> | <identifier> | E + E | (E)
parseArithExpression :: Parser Expr
parseArithExpression = do
  _   <- spaces
  parseLeftAssoc startParser parseAddition
 where
  startParser :: Parser Expr
  startParser = choice
    "Arithmetic expression"
    [ betweenParenthesis parseArithExpression
    , identifierParser
    , integerParser
    ]

  identifierParser :: Parser Expr
  identifierParser = do
    Var <$> parseIdentifier

  integerParser :: Parser Expr
  integerParser = do
    Number <$> parseInteger

  parseAddition :: Parser (Expr -> Expr -> Expr)
  parseAddition = do
    _ <- symbol "+"
    return $ BinaryOp Add

parseBoolExpression :: Parser Expr
parseBoolExpression = do
  _   <- spaces
  parseLeftAssoc startParser parseConjunction
 where
  startParser = choice
    "Boolean expression"
    [ betweenParenthesis parseBoolExpression
    , equalityParser
    , negationParser
    , booleanParser
    ]

  booleanParser :: Parser Expr
  booleanParser = do
    Bool <$> parseBool

  equalityParser :: Parser Expr
  equalityParser = do
    lhs <- parseArithExpression
    _   <- symbol "="
    rhs <- parseArithExpression
    return $ Rel Eq lhs rhs

  negationParser :: Parser Expr
  negationParser = do
    _    <- symbol "!"
    expr <- parseBoolExpression
    return $ Not expr

  parseConjunction :: Parser (Expr -> Expr -> Expr)
  parseConjunction = do
    _ <- symbol "&"
    return $ BoolBinaryOp And

parseCommand :: Parser Com
parseCommand = do
  _            <- spaces
  parseRightAssoc startParser sequenceParser
 where
  startParser :: Parser Com
  startParser =
    choice "Command statement"
      [ assignmentParser
      , ifThenParser
      , whileParser
      ]

  assignmentParser :: Parser Com
  assignmentParser = do
    ident <- parseIdentifier
    _     <- symbol ":="
    expr  <- parseArithExpression
    return $ Assignment ident expr

  ifThenParser :: Parser Com
  ifThenParser = do
    _            <- symbol "if"
    p            <- parseBoolExpression
    _            <- symbol "then"
    trueCommand  <- parseCommand
    _            <- symbol "else"
    falseCommand <- parseCommand
    nextCommand  <- optional startParser
    case nextCommand of
      Nothing  -> return $ IfThen p trueCommand falseCommand
      Just com -> return $ Seq (IfThen p trueCommand falseCommand) com

  whileParser :: Parser Com
  whileParser = do
    _           <- symbol "while"
    p           <- parseBoolExpression
    _           <- symbol "do"
    command     <- parseCommand
    nextCommand <- optional startParser
    case nextCommand of
      Nothing  -> return $ While p command
      Just com -> return $ Seq (While p command) com

  sequenceParser :: Parser (Com -> Com -> Com)
  sequenceParser = do
    _ <- symbol ";"
    return Seq

-- The parser for the entire program
programParser :: Parser Com
programParser = parseCommand

-- Helper function to run the parser on the given string
run :: Parser a -> String -> Either ParseError a
run p s = snd $ runState (runParser go) s
 where
  go = do
    result <- p
    parseEof
    return result

parseProgram :: String -> Either ParseError Com
parseProgram = run programParser

