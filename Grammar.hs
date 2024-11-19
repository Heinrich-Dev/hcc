{-
Henry Boekhoff - 11/13/24
SOURCES:
ANSI C Yacc Grammar - https://www.quut.com/c/ANSI-C-grammar-y.html - Basically stole
the grammar from C from looking at this Yacc file.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Grammar where

import Parser
import Control.Applicative
import Prelude


data Expression = Integer Int
                | Float Float
                | Add Expression Expression
                | Mul Expression Expression
                deriving (Show)

expression :: Parser Expression
expression = do x <- term
                parseString "+"
                y <- term
                return (Add x y)
                <|>
                term

term :: Parser Expression
term = do x <- factor
          parseString "*"
          y <- term
          return (Mul x y)
          <|>
          factor

factor :: Parser Expression
factor = do parseString "("
            x <- term
            parseString ")"
            return x
            <|>
         do x <- parseTokenFloat
            return (Float x)
            <|>
         do x <- parseTokenInt
            return (Integer x)
