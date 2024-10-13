{-
Sources:
Hoogle - https://hoogle.haskell.org
Monadic Parsing Paper - https://www.cs.tufts.edu/comp/150FP/archive/graham-hutton/monadic-parsing-jfp.pdf
JSON Parser from Scratch in Haskell - https://www.youtube.com/watch?v=N9RUqGYuGfw&t=5711s
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import System.IO ()
import System.Environment (getArgs)
import Control.Monad ()

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
-- Parser takes a string and returns a list of the parsed item and the remaining string
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser a) = Parser $ \input -> do -- using do notation since Maybe is a monad
        (x, input') <- a input
        Just (f x, input')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \input -> Just (a, input) -- Given a, return a, don't consume input.

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser a1) <*> (Parser a2) = Parser $ \input -> do -- using do notation since Maybe is a monad
        (f, input') <- a1 input
        (a, input'') <- a2 input'
        Just (f a, input'')

-- x is expected character
parseChar :: Char -> Parser Char
parseChar x = Parser $ \input -> case input of
  c : cs | c == x -> Just(c, cs)
  _ -> Nothing

parseString :: [Char] -> Parser [Char]
parseString = traverse parseChar -- this is what i did all the funny applicative functor stuff for.

main :: IO ()
main = undefined