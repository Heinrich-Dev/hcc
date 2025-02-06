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
emitInstruction (Add x y) = emitInstruction x ++ emitInstruction y ++ "ADD\n"
emitInstruction (Mul x y) = emitInstruction x ++ emitInstruction y ++ "MUL\n"
emitInstruction (Integer x) = "PUSH $" ++ show x ++ "\n"
emitInstruction (Float x) = "PUSH $" ++ show x ++ "\n"
