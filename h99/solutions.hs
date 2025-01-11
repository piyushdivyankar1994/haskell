themselvesTimes :: [Int] -> [Int]
themselvesTimes (x : xs) = (replicate x x) ++ themselvesTimes xs