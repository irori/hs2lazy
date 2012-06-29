even :: [a] -> [a]
even [] = []
even [x] = []
even (x:y:z) = y : even z

main :: Stream -> Stream
main stdin = toStream $ unlines $ even $ lines $ fromStream stdin
