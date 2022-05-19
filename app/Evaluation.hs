module Evaluation where

type Identifier = String

type ErrorMsg = String

data Expr
    = Bool Bool
    | Equals Expr Expr
    | And Expr Expr
    | Not Expr
    | Number Int
    | Var Identifier
    | Plus Expr Expr
    deriving (Show)

data Value
    = NumberVal Int
    | BoolVal Bool
    deriving (Show)

data Com
    = Assignment Identifier Expr
    | IfThen Expr Com Com
    | While Expr Com
    | Seq Com Com
    deriving (Show)

type Environment = [(Identifier, Value)]

handleUnbound :: Value
handleUnbound = NumberVal 0

{-
newtype Evaluator' = Ev { runEvaluator :: Environment -> Either ErrorMsg (a, Environment) }

instance Functor Evaluator' where
    fmap = liftM

instance Monad Evaluator' where

-}

findVar :: Environment -> Identifier -> Maybe Value
findVar [] _ = Nothing
findVar ((key, val) : xs) ident
    | ident == key = Just val
    | otherwise = findVar xs ident

insertVar :: Environment -> Identifier -> Value -> Environment
insertVar [] ident newVal = [(ident, newVal)]
insertVar ((key, _) : xs) ident newVal
    | key == ident = (key, newVal) : xs
    | otherwise = insertVar xs ident newVal

newtype Evaluator a = Ev (Either ErrorMsg a)

instance Show a => Show (Evaluator a) where
    show (Ev a) = show a

instance Functor Evaluator where
    fmap _ (Ev (Left err)) = Ev $ Left err
    fmap f (Ev (Right a)) = Ev $ Right $ f a

instance Monad Evaluator where
    return x = Ev $ Right x

    (>>=) (Ev (Left err)) _ = Ev $ Left err
    (>>=) (Ev (Right a)) f = f a

 -- Added so that we compile the eval function, but should never actually be used
instance MonadFail Evaluator where
    fail msg = Ev $ Left msg

instance Applicative Evaluator where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

eval :: Expr -> Environment -> Evaluator Value
eval (Bool a) _ = do
    return $ BoolVal a
eval (Equals a b) env = do
    NumberVal lhs <- guardNumVal =<< eval a env
    NumberVal rhs <- guardNumVal =<< eval b env
    return $ BoolVal (lhs == rhs)
eval (And a b) env = do
    BoolVal lhs <- guardBoolVal =<< eval a env
    BoolVal rhs <- guardBoolVal =<< eval b env
    return $ BoolVal (lhs && rhs)
eval (Not a) env = do
    BoolVal e <- guardBoolVal =<< eval a env
    return $ BoolVal (not e)
eval (Number a) _ = do
    return $ NumberVal a
eval (Plus a b) env = do
    NumberVal lhs <- guardNumVal =<< eval a env
    NumberVal rhs <- guardNumVal =<< eval b env
    return $ NumberVal (lhs + rhs)
eval (Var ident) env = 
    guardAnyValue $ findVar env ident

guardMessage :: String -> String -> ErrorMsg
guardMessage expected encountered
    = "Expected '" <> expected <> "', but encountered '" <> encountered <> "'"

guardAnyValue :: Maybe Value -> Evaluator Value
guardAnyValue (Just v) = Ev $ Right  v
guardAnyValue Nothing = Ev $ Left "Value expected, but nothing provided"

guardNumVal :: Value -> Evaluator Value
guardNumVal (NumberVal a) = Ev $ Right $ NumberVal a
guardNumVal v = Ev $ Left $ guardMessage "NumberVal" $ show v

guardBoolVal :: Value -> Evaluator Value
guardBoolVal (BoolVal a) = Ev $ Right $ BoolVal a
guardBoolVal v = Ev $ Left $ guardMessage "BoolVal" $ show v

exec :: Com -> Environment -> Environment
exec (Assignment ident expr) env = _


