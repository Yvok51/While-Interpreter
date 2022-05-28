module Evaluation where

import qualified Data.Map as M
import Data.Foldable (Foldable(foldl'))
import Data.Map (fromList)

type Identifier = String

type ErrorMsg = String

data Expr
    = Bool Bool
    | Rel RelOp Expr Expr
    | BoolBinaryOp BoolOp Expr Expr
    | Not Expr
    | Number Int
    | Var Identifier
    | BinaryOp Op Expr Expr
    deriving (Eq, Show)

data RelOp
    = Eq
    | Lt
    | Gt
    deriving (Eq, Show)

data BoolOp
    = And
    | Or
    deriving (Eq, Show)

data Op
    = Add
    | Sub
    | Mul
    | Div
    deriving (Eq, Show)

data Value
    = NumberVal Int
    | BoolVal Bool
    deriving (Eq, Show)

data Com
    = Assignment Identifier Expr
    | IfThen Expr Com Com
    | While Expr Com
    | Seq Com Com
    | Skip
    deriving (Eq, Show)

type Environment = M.Map Identifier Value

emptyEnvironment :: Environment
emptyEnvironment = M.empty

envInsert :: Identifier -> Value -> Environment -> Environment
envInsert = M.insert

envLookup :: Identifier -> Environment -> Maybe Value
envLookup = M.lookup

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
    case envLookup ident env of
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
    set (envInsert ident val env)
    return ()

eval :: Expr -> Interpreter Value
eval (Bool a) = do
    return $ BoolVal a
eval (Rel op a b) = do
    lhs <- guardNumVal =<< eval a
    rhs <- guardNumVal =<< eval b
    case op of 
        Eq -> return $ BoolVal (lhs == rhs)
        Lt -> return $ BoolVal (lhs < rhs)
        Gt -> return $ BoolVal (lhs > rhs)
eval (BoolBinaryOp op a b) = do
    lhs <- guardBoolVal =<< eval a
    rhs <- guardBoolVal =<< eval b
    case op of
        And -> return $ BoolVal (lhs && rhs)
        Or -> return $ BoolVal (lhs || rhs)
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
        _ -> throwError "If statement predicate requires a boolean value"
exec (While expr com) = do
    val <- eval expr
    case val of
        BoolVal True -> do
            exec com
            exec (While expr com)
        BoolVal False -> return ()
        _ -> throwError "While statement predicate requires a boolean value"
exec (Seq comFirst comSecond) = do
    exec comFirst
    exec comSecond
exec Skip = do
    return ()

execProgram :: Com -> Interpreter ()
execProgram = exec

execProgramEnv :: Com -> Environment -> Interpreter ()
execProgramEnv com env = do
    set env
    exec com

getEnv :: Interpreter () -> Maybe Environment
getEnv (Inter f) = case f emptyEnvironment of
    Left _ -> Nothing
    Right ((), env) -> Just env

-- ==== Testing ====

block :: [Com] -> Com
block [] = Seq Skip Skip
block (x : xs) = foldl' Seq x xs

false :: Expr
false = Bool False

true :: Expr
true = Bool True 

(|=) :: Identifier -> Expr -> Com
x |= e = Assignment x e -- := did not work :(

instance Num Expr where
    (+) = BinaryOp Add
    (-) = BinaryOp Sub
    (*) = BinaryOp Mul
    fromInteger a = Number $ fromInteger a

(/) :: Expr -> Expr -> Expr
(/) = BinaryOp Div

(===) :: Expr -> Expr -> Expr
(===) = Rel Eq
(!=) :: Expr -> Expr -> Expr
(!=) a b = Not $ Rel Eq a b 

lt :: Expr -> Expr -> Expr
lt = Rel Lt
gt :: Expr -> Expr -> Expr
gt = Rel Gt

-- ==== examples ====
example1 :: Com
example1 = block
    [ "L" |= (2 + 1)
    ] 
-- >>> exec example1

example2 :: Com
example2 = block
    [ "L" |= (2 + 2)
    , "L" |= (Var "L" + 1)
    ]
-- >>> exec example2

example3 :: Com
example3 = block 
    [ "L" |= (2 + 2)
    , IfThen false
        (block [ "L" |= (Var "L" + 1) ])
        (block [ "L" |= (Var "L" + 2) ])
    ]
-- >>> exec example3

example4 :: Com
example4 = block
    [ "I" |= 1
    , "Cycles" |= 5
    , While (Var "Cycles" != 0)
        (block 
        [ "Cycles" |= (Var "Cycles" - 1)
        , "I" |= (Var "I" + Var "I")
        ])
    ]
-- >>> exec example4

