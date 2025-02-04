module Generator where

import Grammar
import System.IO

generate :: Expression -> IO()
generate expr = do
    handle <- createFile "test.asm"
    createFormat handle
    createSectionData handle
    createSectionText handle

createFile :: String -> IO Handle
createFile name = openFile name WriteMode

createFormat :: Handle -> IO()
createFormat handle = do hPutStrLn handle "format ELF64 executable 3"
                         hPutStrLn handle "segment readable executable"

createSectionData :: Handle -> IO()
createSectionData handle = hPutStrLn handle "section '.data' writable"

createSectionText :: Handle -> IO()
createSectionText handle = hPutStrLn handle "section '.text' executable"