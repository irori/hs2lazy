evenList :: [a] -> [a]
evenList [] = []
evenList [x] = []
evenList (x:y:z) = y : evenList z

main = interact (unlines . evenList . lines)
