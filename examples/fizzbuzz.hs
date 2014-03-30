fb n | mod n 15 == 0 = "FizzBuzz"
     | mod n 5  == 0 = "Buzz"
     | mod n 3  == 0 = "Fizz"
     | otherwise     = show n

fizzbuzz n | n > 100 = []
           | otherwise = fb n ++ '\n' : fizzbuzz (n+1)

main = putStr $ fizzbuzz 1
