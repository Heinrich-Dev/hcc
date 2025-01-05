module Main where

import Parser
import Grammar
import Generator
import System.IO
import Control.Monad
import System.Exit
import System.Environment

main = do
    args    <- getArgs
    content <- checkArgs args
    let tree = parse expression content
    print tree

checkArgs ["-h"] = usage >> exitSuccess
checkArgs ["-v"] = version >> exitSuccess
checkArgs [] = getContents
checkArgs [filename] = readFile filename
checkArgs _ = exitFailure --this shouldnt happen

usage = putStrLn "Usage: hcc [-vh] [file]"
version = putStrLn "Version: Henry's C Compiler 0.1"