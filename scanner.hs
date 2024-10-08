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
           | INLINE_COMMENT | GROUP_COMMENT

main :: IO ()
main = do
    contents <- readFile "hello.c"
    print contents