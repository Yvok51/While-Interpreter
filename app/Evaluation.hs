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

newtype Interpreter a = Inter { runInterpreter :: Environment -> Either ErrorMsg (a, Environment) }

instance Show a => Show (Interpreter a) where
    show (Inter f) = show (f []) -- TODO: better option possible?

instance Functor Interpreter where
    fmap f inter = Inter $ \env -> case runInterpreter inter env of
        Right (a, newEnv) -> Right (f a, newEnv)
        Left  msg         -> Left msg

instance Monad Interpreter where
    return a = Inter $ \env -> Right (a, env)

    (>>=) inter f = Inter $ \env -> case runInterpreter inter env of
        Right (a, newEnv) -> runInterpreter (f a) newEnv
        Left  err         -> Left err

instance Applicative Interpreter where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

 -- Added so that we compile the eval function, but should never actually be used
instance MonadFail Interpreter where
    fail msg = Inter $ const $ Left msg

get :: Interpreter Environment
get = Inter $ \env -> Right (env, env)

set :: Environment -> Interpreter ()
set env = Inter $ const $ Right ((), env)

findVar :: Environment -> Identifier -> Maybe Value
findVar [] _ = Nothing
findVar ((key, val) : xs) ident | ident == key = Just val
                                | otherwise    = findVar xs ident

insertVar :: Environment -> Identifier -> Value -> Environment
insertVar [] ident newVal = [(ident, newVal)]
insertVar ((key, val) : xs) ident newVal
    | key == ident = (key, newVal) : xs
    | otherwise    = (key, val) : insertVar xs ident newVal

readVariable :: Identifier -> Interpreter Value
readVariable ident = Inter $ \env -> case findVar env ident of
    Nothing -> Left
        (  "Variable "
        ++ ident
        ++ " is not defined. Currently defined variables: "
        ++ show env
        )
    Just val -> Right (val, env)

writeVariable :: Identifier -> Value -> Interpreter ()
writeVariable ident val = Inter $ \env -> Right ((), insertVar env ident val)

eval :: Expr -> Interpreter Value
eval (Bool a) = do
    return $ BoolVal a
eval (Equals a b) = do
    NumberVal lhs <- guardNumVal =<< eval a
    NumberVal rhs <- guardNumVal =<< eval b
    return $ BoolVal (lhs == rhs)
eval (And a b) = do
    BoolVal lhs <- guardBoolVal =<< eval a
    BoolVal rhs <- guardBoolVal =<< eval b
    return $ BoolVal (lhs && rhs)
eval (Not a) = do
    BoolVal e <- guardBoolVal =<< eval a
    return $ BoolVal (not e)
eval (Number a) = do
    return $ NumberVal a
eval (Plus a b) = do
    NumberVal lhs <- guardNumVal =<< eval a
    NumberVal rhs <- guardNumVal =<< eval b
    return $ NumberVal (lhs + rhs)
eval (Var ident) = readVariable ident

guardMessage :: String -> String -> ErrorMsg
guardMessage expected encountered =
    "Expected '" <> expected <> "', but encountered '" <> encountered <> "'"

{-
guardAnyValue :: Maybe Value -> Interpreter Value
guardAnyValue (Just v) = Inter $ \env -> Right (v, env)
guardAnyValue Nothing = Inter $ const $ Left "Value expected, but nothing provided"
-}

guardNumVal :: Value -> Interpreter Value
guardNumVal (NumberVal a) = Inter $ \env -> Right (NumberVal a, env)
guardNumVal v = Inter $ const $ Left $ guardMessage "NumberVal" $ show v

guardBoolVal :: Value -> Interpreter Value
guardBoolVal (BoolVal a) = Inter $ \env -> Right (BoolVal a, env)
guardBoolVal v = Inter $ const $ Left $ guardMessage "BoolVal" $ show v

exec :: Com -> Interpreter ()
exec (Assignment ident expr) = do
    val <- eval expr
    writeVariable ident val
exec (IfThen expr comTrue comFalse) = do
    val <- eval expr
    case val of
        BoolVal True -> exec comTrue
        BoolVal False -> exec comFalse
        _ -> Inter $ const $ Left "If statement requires a boolean value"
exec (While expr com) = do
    val <- eval expr
    case val of
        BoolVal True -> do
            exec com
            exec (While expr com)
        BoolVal False -> return () -- TODO: seems fishy
        _ -> Inter $ const $ Left "While statement requires a boolean value"
exec (Seq comFirst comSecond) = do
    exec comFirst
    exec comSecond

-- examples
-- >>> exec (Assignment "L" (Plus (Number 2) (Number 2)))

-- >>> exec (Seq (Assignment "L" (Plus (Number 2) (Number 2))) (Assignment "L" (Plus (Var "L") (Number 1))))

{-
L := 2 + 2;
If false Then
    L := L + 1
Else
    L := L + 2
-}
-- >>> exec (Seq (Assignment "L" (Plus (Number 2) (Number 2))) (IfThen (Bool False) (Assignment "L" (Plus (Var "L") (Number 1))) (Assignment "L" (Plus (Var "L") (Number 2)))))

{-
L := 4;
If !(L = 4) Then
    L := L + 1
Else
    L := L + 2
L := L + (-1)
-}
-- >>> exec (Seq (Assignment "L" (Number 4)) (Seq (IfThen (Not (Equals (Var "L") (Number 4))) (Assignment "L" (Plus (Var "L") (Number 1))) (Assignment "L" (Plus (Var "L") (Number 2)))) (Assignment "L" (Plus (Var "L") (Number (-1))))))

{-
I := 1;
Cycles := 3;
While !(Cycles = 0) Then
    Cycles := Cylcles + (-1);
    I := I + 2
-}
-- >>> exec (Seq (Assignment "I" (Number 1)) (Seq (Assignment "Cycles" (Number 3)) (While (Not (Equals (Var "Cycles") (Number 0))) (Seq (Assignment "Cycles" (Plus (Var "Cycles") (Number (-1)))) (Assignment "I" (Plus (Var "I") (Number 2)))))))

