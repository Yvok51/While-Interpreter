module Main where

import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Monad                  ( when
                                                , unless
                                                )

import           Evaluation                     ( execProgram
                                                , Interpreter
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

repl :: IO ()
repl = do
    line <- read'
    unless (line == ":q") $ case parseProgram line of 
        Left err -> print err >> repl
        Right com -> print (execProgram com) >> repl

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [file] -> do
            contents <- readFile file
            putStrLn $ eval' contents
        _ -> do
            putStrLn "Wrong number of arguments"

