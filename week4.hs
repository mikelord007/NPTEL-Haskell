{-

1. Define a function f1 :: [Int] -> [Int] which takes a list l of nonnegative 
numbers as input, and replaces each n in l by 3*n if n is a power of 3, 
and by 0 if it is not a power of 3. 
Examples:
  f1 [] = []
  f1 [1] = [3]
  f1 [1, 2, 3] = [3, 0, 9]
  f1 [0, 2, 4, 6] = [0, 0, 0, 0]

2. For a list l, define S(l) to be the set of all indices i of l (remember that 
indices start from 0) such that l!!i > l!!(i+1). Define a function 
f2 :: [Int] -> [Int] which takes a nonempty list l of 
integers as input and outputs a S(l) in order.
Examples:
  f2 [] = []
  f2 [1] = []
  f2 [1, 2, 3, 2, 1] = [2, 3]
  f2 [1, 2, 3, 4, 5, 6] = []

3. Define a function f3 :: [Int] -> [Int] that removes adjacent duplicates. 
i.e. if the same element occurs n times contiguously, we retain only one copy.
Examples:
  f3 [1, 1, 1, 2, 2, 3, 3, 3, 3] = [1, 2, 3]
  f3 [1, 2, 1, 2, 3, 1, 1, 2, 2] = [1, 2, 1, 2, 3, 1, 2]

4. Define a function f4 :: [Int] -> [[Int]] that partitions the list into all 
its upruns. An uprun is a maximal non-decreasing segment of the given list. 
Examples:
f4 [] = []
f4 [5] = [[5]]
f4 [1, 2, 3, 4, 5] = [[1,2,3,4,5]]
f4 [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1] = [[1,2,3,4,5,6],[5],[4],[3],[2],[1]]


-}


f1 :: [Int] -> [Int]
f1 [] = []
f1 (x:xs)
    | pow3 x = x*3:f1 xs
    | otherwise = 0:f1 xs
    where pow3 :: Int -> Bool
          pow3 x
            | x == 0 = True
            | x == 1 = True
            | x `mod` 3 == 0 = pow3 (x `div` 3)
            | otherwise = False

f2 :: [Int] -> [Int]
f2 xs = [i-1 | i <- [1..(length xs - 1)], xs!!(i-1)>xs!!i ]


f3 :: [Int] -> [Int]
f3 [] = []
f3 [x] = [x]
f3 (x:y:xs)
    | x==y = f3 (y:xs)
    | otherwise = x:f3 (y:xs)


f4 :: [Int] -> [[Int]]
f4 [] = []
f4 xs = foldl k [] xs
    where k [] n = [[n]]
          k xs n = if (last . last) xs <= n
                     then init xs ++ [last xs ++ [n]]
                   else xs++[[n]]