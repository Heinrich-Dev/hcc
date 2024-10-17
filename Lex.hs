{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lex where

import Control.Applicative ()
import Data.Char ()

data Token = DATA_TYPE | ID | L_PAREN | R_PAREN | R_BRACE | L_BRACE | KEYWORD String | SEMICOLON

newtype Parser a = P (String -> Maybe(a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \input -> case parse p input of
        Nothing -> Nothing
        Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \input -> Just(a, input)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    fx <*> y = P $ \input -> case parse fx input of
        Nothing -> Nothing
        Just (c, rest) -> parse (fmap c y) rest

parseChar :: Char -> Parser Char
parseChar x = P $ \input -> case input of
    c:cs | c == x -> Just(c, cs)
    _ -> Nothing

parseString :: String -> Parser String
parseString = traverse parseChar