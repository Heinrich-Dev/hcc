module Main where

import Parser
import Grammar
import Linear
import Generator
import System.IO
import Control.Monad
import System.Exit
import System.Environment

main = do
    args    <- getArgs
    content <- checkArgs args
    let Just(tree, str) = parse expression content
    print tree
    checkParse str
    linearify tree

checkArgs ["-h"] = usage >> exitSuccess
checkArgs ["-v"] = version >> exitSuccess
checkArgs [] = getContents
checkArgs [filename] = readFile filename
checkArgs _ = exitFailure --this shouldnt happen ;)

usage = putStrLn "Usage: hcc [-vh] [file]"
version = putStrLn "Version: Henry's C Compiler 0.1"

checkParse :: String -> IO()
checkParse [] = putStrLn "Successful parse..."
checkParse str = putStrLn "Some content unparsed, exiting..." >> exitFailure