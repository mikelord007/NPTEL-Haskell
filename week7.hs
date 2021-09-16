{-
    In this assignment you have to submit a standalone Haskell program that can
    be compiled using the ghc compiler.

    The input to the program will be multiple lines. Each input line is guaranteed
    to be a single word, with no beginning or trailing spaces. On reading a line,
    your program should print "Y"  if the word is a palindrome (after converting
    every letter to lowercase), and "N" if not.

    A palindrome is a string that is the same as its reverse.

    Here is a sample run:

    Input
    -----
    abba
    Level
    Hi
    Malayalam

    Output
    ------
    Y
    Y
    N
    Y
-}

import Data.Char

checkPali :: String -> IO ()
checkPali "" = putStrLn "Y"
checkPali s = checkBool [toLower (s !! i) == toLower (s !! (length s - 1 - i)) | i <- [0 .. (length s `div` 2 - 1)]]
  where
    checkBool s =
      if and s
        then putStrLn "Y"
        else putStrLn "N"

main = do
  inp <- getLine
  checkPali inp
  main