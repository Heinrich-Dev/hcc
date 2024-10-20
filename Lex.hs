{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Lex where

import Control.Applicative
import Data.Char

data Token = DATA_TYPE | ID | L_PAREN | R_PAREN | R_BRACE | L_BRACE | KEYWORD String | SEMICOLON

newtype Parser a = P (String -> Maybe(a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Monad Parser where
    return :: a -> Parser a
    return a = P $ \input -> Just (a, input)

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= f = P $ \input -> case parse a input of
        Just(x, xs) -> parse (f x) xs
        _ -> Nothing

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

instance Alternative Parser where
    empty :: Parser a
    empty = P $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P $ \input -> parse p1 input <|> parse p2 input

get :: Parser Char
get = P $ \case
    (x:xs) -> Just(x, xs)
    _ -> Nothing

parseChar :: Char -> Parser Char
parseChar x = P $ \case
    c:cs | c == x -> Just(c, cs)
    _ -> Nothing

parseString :: String -> Parser String
parseString = traverse parseChar

predicate :: (Char -> Bool) -> Parser Char
predicate f = do 
    x <- get
    if f x
        then return x 
        else empty

parseDigit :: Parser Char
parseDigit = predicate isDigit

parseNumber :: Parser String
parseNumber = P $ \input -> Just(span isDigit input)