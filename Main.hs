{-
Sources: 
Monadic Parsing Paper - https://www.cs.tufts.edu/comp/150FP/archive/graham-hutton/monadic-parsing-jfp.pdf
JSON Parser from Scratch in Haskell - https://www.youtube.com/watch?v=N9RUqGYuGfw&t=5711s
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Main where

import System.IO ()
import System.Environment (getArgs)
import Control.Monad (liftM, ap)

data Token = TYPE | ID | POINTER
           | L_PAREN | R_PAREN | L_BRACE | R_BRACE | L_BRACKET | R_BRACKET | SEMICOLON | COLON -- single character tokens
           | PLUS | MINUS | MULTI | DIV -- arithmetic operations
           | AND | OR | XOR | SHIFT_LEFT | SHIFT_RIGHT | SHIFT_RIGHT_GETS | SHIFT_LEFT_GETS | NOT -- bitwise operations
           | B_AND | B_OR | B_XOR | B_EQUAL | B_LESS_EQUAL | B_GREATER_EQUAL | B_NOT -- boolean operations
           | AUTO | DOUBLE | INT | STRUCT | BREAK | ELSE | LONG | SWITCH | CASE | ENUM | REGISTER | TYPEDEF | CHAR
           | EXTERN | RETURN | UNION | CONST | FLOAT | SHORT | UNSIGNED | CONTINUE | FOR | SIGNED | VOID | DEFAULT
           | GOTO | SIZEOF | VOLATILE | DO | IF | STATIC | WHILE | INLINE -- keywords
           | INLINE_COMMENT | GROUP_COMMENT -- comments
           | INCLUDE | PRAGMA | ASM -- compiler directives
           deriving (Show)

type Error = String
-- Parser takes a string and returns a list of the parsed item and the remaining string
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parseChar :: Parser Char
parseChar = Parser $ \inp -> case inp of
  [] -> Nothing
  c : cs -> Just(c, cs)


main :: IO ()
main = undefined