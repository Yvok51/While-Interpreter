module Main where

import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Monad                  ( when
                                                , unless
                                                )

import           Evaluation                     ( execProgram
                                                , Environment
                                                , getEnv
                                                , emptyEnvironment
                                                , execProgramEnv
                                                )
import           Parser                         ( parseProgram )

read' :: IO String
read' = do
    putStr "WHILE> "
    hFlush stdout
    getLine

eval' :: String -> String
eval' input = case parseProgram input of
    Left  err -> show err
    Right com -> show $ execProgram com

repl :: Environment -> IO ()
repl env = do
    line <- read'
    unless (line == ":q") $ case parseProgram line of 
        Left err -> do
            print err 
            repl env
        Right com -> do
            let inter = execProgramEnv com env
            print inter
            let mNewEnv = getEnv inter
            case mNewEnv of
                Nothing -> repl env
                Just newEnv -> repl newEnv

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl emptyEnvironment 
        [file] -> do
            contents <- readFile file
            putStrLn $ eval' contents
        _ -> do
            putStrLn "Wrong number of arguments"

