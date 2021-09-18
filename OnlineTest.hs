{-
    1. Write a function f1 :: [a] -> Int -> [a] that takes a list and a positive number n, and drops elements of the list at index i*n-1, for i > 0.
Remember that list indices start at 0.

Test cases:
f1 [1,2,3,4,5] 2 = [1,3,5]
f1 [2,4,6,8,10] 1 = []
f1 [1,3,5,7,9,11,13,15] 4 = [1,3,5,9,11,13]

2. Define a function f2 :: Int -> Int that takes a non-negative integer and places its rightmost digit at the leftmost position.

Test cases:
f2 1 = 1
f2 123 = 312
f2 67890 = 6789
f2 678900 = 67890
f2 678901 = 167890

-}

f1 :: [a] -> Int -> [a]
f1 xs n = [xs !! i | i <- [0 .. length xs - 1], i `notElem` [(n - 1), (n - 1) + n .. length xs - 1]]

f2 :: Int -> Int
f2 0 = 0
f2 n = (n `div` 10) + (n `mod` 10 * round (10 ^^ (noOfDigits n - 1)))
  where
    noOfDigits 0 = 0
    noOfDigits n = 1 + noOfDigits (n `div` 10)