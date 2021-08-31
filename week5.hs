{-
 
    1. Define a function subSeq :: String -> String -> Bool which checks whether 
the first argument is a subsequence of the second. A subsequence is obtained by 
deleting some letters in a string and retaining the other characters in the same 
order as in the original string.

Test cases:
subSeq "ab" "abc" = True
subSeq "ab" "acb" = True
subSeq "ab" "bca" = False
subSeq ""   "bea" = True
subSeq "ba" "ba"  = True

2. Define a function subWord :: String -> String -> Bool which checks whether 
the first argument is a subword of the second. A subword is obtained by deleting 
some number (possibly 0) of letters at the left end and right end in a string 
and retaining the other characters in the same order.

Test cases:
subWord "ab" "abc" = True
subWord "ab" "acb" = False
subWord "ca" "bca" = True
subWord ""   "bea" = True
subWord "ba" "ba"  = True

3. A two-dimensional matrix can be represented as a list of rows, each row 
itself being a list of elements. So in general it is of type [[a]]. Not every 
list of lists is a matrix, though. For instance, [[1,2,3], [], [2,4]] is a list 
of three lists, each of a different size.

(a) Define a function isMatrix :: [[a]] -> Bool that checks if a list of lists 
is a valid matrix (nonzero number of rows, each of the same nonzero length).

Test cases:
isMatrix [] = False
isMatrix [[],[],[]] = False
isMatrix [[2,3], [4,5], [6,7]] = True 
isMatrix [[2,3,4,5,6,7]] = True

(b) A square matrix is one where the number of rows is equal to the number of 
columns. Define a function isSquareMatrix :: [[a]] -> Bool that checks if a 
list of lists is a square matrix.

Test cases:
isSquareMatrix [] = False
isSquareMatrix [[]] = False
isSquareMatrix [[1]] = True
isSquareMatrix [[1,2,3],[4,5,6],[7,8,9]] = True
isSquareMatrix [[1,2,3,4],[5,6,7,8],[9,10,11,12]] = False

(c) Two matrices are addable if they have the same number of rows and same 
number of columns. Define a function addable :: [[a]] -> [[a]] -> Bool that 
checks if two matrices are addable.

Test cases: 
addable [[1,2],[3,4]] [[1,2],[3,4]] = True
addable [[1,2],[3,4]] [[5,6,7],[8,9,10]] = False
addable [[1,2],[3,4]] [[1,2],[3,4],[3,4]] = False

(d) Define a function addMatrices :: [[Int]] -> [[Int]] -> [[Int]] that computes 
the sum of the input matrices.

Test cases:
addMatrices [[1,2]] [[3,4]] = [[4,6]]
addMatrices [[1,2],[3,4]] [[1,2],[3,4]] = [[2,4],[6,8]]

(e) Matrix m1 is multiplyable with matrix m2 if the number of columns in m1 is 
the same as the number of rows in m2. Define a function 
multiplyable :: [[a]] -> [[a]] -> Bool that checks if matrix m1 is 
multiplyable with m2.

Test cases:
multiplyable [[1,2,3],[4,5,6]] [[1,2],[3,4]] = False
multiplyable [[1,2,3],[4,5,6],[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = True
 
(f) Define a function multiplyMatrices :: [[Int]] -> [[Int]] -> [[Int]] that 
computes the product of the input matrices.

Test cases:
multiplyMatrices [[1,2],[3,4]] [[1,2,3],[4,5,6]] = [[9,12,15],[19,26,33]]
multiplyMatrices [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = [[22,28],[49,64]]


-}


subSeq :: String -> String -> Bool
subSeq "" xs = True
subSeq s "" = False
subSeq s xs
    | head s == head xs = subSeq (tail s) (tail xs)
    | otherwise = subSeq s ( tail xs )

subWord :: String -> String -> Bool 

subWord "" xs = True
subWord s "" = False
subWord s xs
        | head s == head xs = exactmatch s xs
        | otherwise = subWord s (tail xs)
        where exactmatch s xs
                    | s == take (length s) xs = True 
                    | otherwise = subWord s (tail xs)


