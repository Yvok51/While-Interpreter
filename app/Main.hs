module Main where

import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

import           Evaluation                     ( execProgram )
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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            putStrLn $ eval' contents
        _ -> do
            putStrLn "Wrong number of arguments"

