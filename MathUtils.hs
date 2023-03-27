module MathUtils where
    pow :: Num a => a -> Int -> a
    pow num power | power <= 0 = 1
        | otherwise = num * pow num (power - 1)
    
    
    summation :: Num a => [a] -> a
    summation list = case list of
                         [] -> 0
                         (x:xs) -> x + summation xs
                         
    distance :: Num a => a -> a -> a
    distance first_num second_num = abs (first_num - second_num)
