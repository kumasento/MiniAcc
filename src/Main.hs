import AccCodeGen
import AccType
import AccAST
import AccRun

main :: IO ()
main = do
    res <- runAccMonad 
        (AccAST.zipWith 
            Add 
            (use (ArrayT 10000 [1..10000])) 
            (AccAST.zipWith 
                Add 
                (use (ArrayT 10000 [1..10000])) 
                (use (ArrayT 10000 [1..10000]))))
    return ()
