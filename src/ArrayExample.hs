import Array

main = do
    let arrL = (fromList (sDIM2 2 3) Int [1..6])
    putStrLn $ "Array+List: " ++ (show arrL)
