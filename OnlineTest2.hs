{-
1. Define a function f1 :: [a] -> [a] that takes a list l, and replicates each element (l!!i) (i+1) times. Remember that indices start at 0.

Test cases:
f1 [] = []
f1 [1] = [1]
f1 [1,4,7] = [1,4,4,7,7,7]

2. Define a function f2 :: String -> String -> Bool that checks if the first string is a subsequence (need not be contiguous) of the second.

Test Cases:
f2 "ab" "abc" = True
f2 "ac" "abc" = True
f2 "ab" "bca" = False

-}

f1 :: [a] -> [a]
f1 xs = [xs !! i | i <- [0 .. length xs - 1], j <- [1 .. i + 1]]

f2 :: String -> String -> Bool
f2 "" _ = True
f2 _ "" = False
f2 xs ys
  | head xs == head ys = f2 (tail xs) (tail ys)
  | otherwise = f2 xs (tail ys)