module Linear where
import Grammar
import System.IO

linearify :: Expression -> IO()
linearify expr = do 
    handle <- openFile "test.l" WriteMode
    emitLinearIR expr handle
    hClose handle
    putStrLn "Finished generating linear IR..."

emitLinearIR :: Expression -> Handle -> IO()
emitLinearIR expr handle = hPutStrLn handle (emitInstruction expr)

emitInstruction :: Expression -> String
emitInstruction (Add x y) = "ADD " ++ emitInstruction x ++ emitInstruction y ++ "\n"
emitInstruction (Mul x y) = "MUL " ++ emitInstruction x ++ emitInstruction y ++ "\n"
emitInstruction (Integer x) = "LOAD $" ++ show x ++ "\n"
emitInstruction (Float x) = "LOAD $" ++ show x ++ "\n"
