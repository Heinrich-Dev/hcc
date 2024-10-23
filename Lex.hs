{-
Henry Boekhoff - 10/21/24
SOURCES: Functional Parsing by Computerphile - https://www.youtube.com/watch?v=dDtZLm7HIJs (The video and the functional parsing library provided.)
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Lex where

import Control.Applicative
import Data.Char

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

--parseNumber :: Parser String
--parseNumber = P $ \input -> Just(span isDigit input)

parseNumber :: Parser Int
parseNumber = do x <- some parseDigit
                 return (read x :: Int)

parseDec :: Parser Float
parseDec = do x <- some parseDigit
              x2 <- parseString "."
              x3 <- some parseDigit
              let y = x ++ x2 ++ x3
              return (read y :: Float)

parseInteger :: Parser Int
parseInteger = do 
                parseChar '-'
                x <- parseNumber
                return (-x)
                <|>
                parseNumber

parseFloat :: Parser Float
parseFloat = do
              parseChar '-'
              x <- parseDec
              return(-x)
              <|>
              parseDec

parseSpace :: Parser Char
parseSpace = predicate isSpace

parseWhiteSpace :: Parser () --this includes new lines thank god
parseWhiteSpace = do 
                    some parseSpace
                    return ()