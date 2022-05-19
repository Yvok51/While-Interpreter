{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Parser where

import           Data.Char                      ( isDigit
                                                , isSpace
                                                )

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f stavovyVypocet = State $ \s ->
    let (s', a) = runState stavovyVypocet s
    in  (s', f a)

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

data ParseError = ParseError
  { errorExpected :: String
  , errorFound    :: String
  }
  deriving Eq

instance Show ParseError where
  show err =
    "expected: " <> errorExpected err <> ", but found: " <> errorFound err

newtype Parser a = Parser {runParser :: State String (Either ParseError a)}

parseEof :: Parser ()
parseEof = Parser $ do
  input <- get
  case input of
    []      -> return $ Right ()
    (c : _) -> return $ Left $ ParseError "end of file" [c]

parseAny :: Parser Char
parseAny = Parser $ do
  input <- get
  case input of
      [] -> return $ Left expectedCharError
      (c : xs) -> do
                  set xs
                  return $ Right c

expectedCharError :: ParseError
expectedCharError = ParseError "any character" "end of file"

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f (Parser p) = Parser $ do
  result <- p
  case result of
      Left err -> return $ Left err
      Right a -> return $ Right $ f a

instance Functor Parser where
  fmap = mapParser

returnParser :: a -> Parser a
returnParser x = Parser $ do
  return $ Right x

andThenParser :: Parser a -> (a -> Parser b) -> Parser b
andThenParser (Parser p) f = Parser $ do
  result <- p
  case result of
    Right x   -> runParser (f x)
    Left  err -> return $ Left err

instance Monad Parser where
  return = returnParser
  (>>=)  = andThenParser

instance Applicative Parser where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ do
  backup <- get
  resultP1 <- p1
  case resultP1 of
      Right a -> return $ Right a
      Left _ -> do
          set backup
          p2

parseError :: String -> String -> Parser a
parseError expected found = Parser $ return $ Left $ ParseError expected found

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description p = do
  c <- parseAny
  if p c then return c
         else parseError description [c]

run :: Parser a -> String -> Either ParseError a
run p s = snd $ runState (runParser go) s
 where
  go = do
    result <- p
    parseEof
    return result

char :: Char -> Parser Char
char c = satisfy [c] (== c)

space :: Parser Char
space = satisfy "space" isSpace

digit :: Parser Char
digit = satisfy "digit" isDigit

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest  <- many p
  return (first : rest)

string :: String -> Parser String
string = mapM char

number :: Parser Int
number = do
  digits <- many1 digit
  return $ read digits

spaces :: Parser String
spaces = many space

symbol :: String -> Parser String
symbol s = do
  result <- string s
  _      <- spaces
  return result

choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch where noMatch = parseError desc "no match"

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
