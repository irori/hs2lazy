main :: Stream -> Stream
main stdin = toStream $ unlines $ reverse $ lines $ fromStream stdin
