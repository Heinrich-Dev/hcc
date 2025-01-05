module Generator where

import Grammar

generate :: Expression -> String
generate expr = "section .data\n" ++ generateData expr ++
                "section .bss\n"  ++ generateBSS expr  ++
                "section .text\n" ++ generateText expr

generateData :: Expression -> String
generateData expr = "section .data\n"

generateBSS :: Expression -> String
generateBSS expr = "section .bss\n"

generateText :: Expression -> String
generateText expr = "section .text\n"