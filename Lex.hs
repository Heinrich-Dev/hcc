module Lex where

data Token = DATA_TYPE | ID | L_PAREN | R_PAREN | R_BRACE | L_BRACE | KEYWORD String | SEMICOLON

type Parser a = String -> Maybe(a, String)

parseChar :: Parser Char
parseChar [] = Nothing
parseChar (x:xs) = Just(x, xs)

parseString :: [Char] -> Parser String
parseString [] = []
