module Evaluation where

import qualified Data.Map as M

type Identifier = String

type ErrorMsg = String

data Expr
    = Bool Bool
    | Equals Expr Expr
    | And Expr Expr
    | Not Expr
    | Number Int
    | Var Identifier
    | BinaryOp Op Expr Expr
    deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data Value
    = NumberVal Int
    | BoolVal Bool
    deriving (Show)

data Com
    = Assignment Identifier Expr
    | IfThen Expr Com Com
    | While Expr Com
    | Seq Com Com
    | Skip
    deriving (Show)

type Environment = M.Map Identifier Value

emptyEnvironment :: Environment
emptyEnvironment = M.empty

handleUnbound :: Value
handleUnbound = NumberVal 0

newtype Interpreter a = Inter { runInterpreter :: Environment -> Either ErrorMsg (a, Environment) }

emptyInterpreter :: Interpreter ()
emptyInterpreter = Inter $ const $ Right ((), emptyEnvironment)

instance Show a => Show (Interpreter a) where
    show (Inter f) = case f emptyEnvironment of
        Left err -> err
        Right (a, env) -> show (a, env)

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


get :: Interpreter Environment
get = Inter $ \env -> Right (env, env)

set :: Environment -> Interpreter ()
set env = Inter $ const $ Right ((), env)

throwError :: ErrorMsg -> Interpreter a
throwError msg = Inter $ const $ Left msg

readVariable :: Identifier -> Interpreter Value
readVariable ident = do
    env <- get
    case M.lookup ident env of
        Nothing -> throwError
            (  "Variable "
            ++ ident
            ++ " is not defined. Currently defined variables: "
            ++ show env
            )
        Just val -> return val

writeVariable :: Identifier -> Value -> Interpreter ()
writeVariable ident val = do
    env <- get
    set (M.insert ident val env)
    return ()

eval :: Expr -> Interpreter Value
eval (Bool a) = do
    return $ BoolVal a
eval (Equals a b) = do
    lhs <- guardNumVal =<< eval a
    rhs <- guardNumVal =<< eval b
    return $ BoolVal (lhs == rhs)
eval (And a b) = do
    lhs <- guardBoolVal =<< eval a
    rhs <- guardBoolVal =<< eval b
    return $ BoolVal (lhs && rhs)
eval (Not a) = do
    e <- guardBoolVal =<< eval a
    return $ BoolVal (not e)
eval (Number a) = do
    return $ NumberVal a
eval (BinaryOp op a b) = do
    lhs <- guardNumVal =<< eval a
    rhs <- guardNumVal =<< eval b
    case op of
      Add -> return $ NumberVal (lhs + rhs)
      Sub -> return $ NumberVal (lhs - rhs)
      Mul -> return $ NumberVal (lhs * rhs)
      Div -> case rhs of
        0 -> throwError "Division by zero"
        _ -> return $ NumberVal (lhs `div` rhs)
eval (Var ident) = readVariable ident

guardMessage :: String -> String -> ErrorMsg
guardMessage expected encountered =
    "Expected '" <> expected <> "', but encountered '" <> encountered <> "'"

guardNumVal :: Value -> Interpreter Int
guardNumVal (NumberVal a) = Inter $ \env -> Right (a, env)
guardNumVal v = throwError $ guardMessage "NumberVal" $ show v

guardBoolVal :: Value -> Interpreter Bool
guardBoolVal (BoolVal a) = Inter $ \env -> Right (a, env)
guardBoolVal v = throwError $ guardMessage "BoolVal" $ show v

exec :: Com -> Interpreter ()
exec (Assignment ident expr) = do
    val <- eval expr
    writeVariable ident val
exec (IfThen expr comTrue comFalse) = do
    val <- eval expr
    case val of
        BoolVal True -> exec comTrue
        BoolVal False -> exec comFalse
        _ -> throwError "If statement requires a boolean value"
exec (While expr com) = do
    val <- eval expr
    case val of
        BoolVal True -> do
            exec com
            exec (While expr com)
        BoolVal False -> return ()
        _ -> throwError "While statement requires a boolean value"
exec (Seq comFirst comSecond) = do
    exec comFirst
    exec comSecond
exec Skip = do
    return ()

execProgram :: Com -> Interpreter ()
execProgram = exec

-- examples
-- >>> exec (Assignment "L" (BinaryOp Add (Number 2) (Number 2)))

-- >>> exec (Seq (Assignment "L" (BinaryOp Add (Number 2) (Number 2))) (Assignment "L" (BinaryOp Add (Var "L") (Number 1))))

{-
L := 2 + 2;
if false Then
    L := L + 1
else
    L := L + 2
-}
-- >>> exec (Seq (Assignment "L" (BinaryOp Add (Number 2) (Number 2))) (IfThen (Bool False) (Assignment "L" (BinaryOp Add (Var "L") (Number 1))) (Assignment "L" (BinaryOp Add (Var "L") (Number 2)))))

{-
L := 4;
if !(L = 4) Then
    L := L + 1
else
    L := L + 2
L := L + (-1)
-}
-- >>> exec (Seq (Assignment "L" (Number 4)) (Seq (IfThen (Not (Equals (Var "L") (Number 4))) (Assignment "L" (BinaryOp Add (Var "L") (Number 1))) (Assignment "L" (BinaryOp Add (Var "L") (Number 2)))) (Assignment "L" (BinaryOp Add (Var "L") (Number (-1))))))

{-
I := 1;
Cycles := 3;
while !(Cycles = 0) do
    Cycles := Cylcles + (-1);
    I := I + 2
-}
-- >>> exec (Seq (Assignment "I" (Number 1)) (Seq (Assignment "Cycles" (Number 3)) (While (Not (Equals (Var "Cycles") (Number 0))) (Seq (Assignment "Cycles" (BinaryOp Add (Var "Cycles") (Number (-1)))) (Assignment "I" (BinaryOp Add (Var "I") (Number 2)))))))



