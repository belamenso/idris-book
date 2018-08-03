module Main

import Average
import Exercises

showAverage : String -> String
showAverage str = "The average word length is: " ++ show (average str) ++ "\n"

{-
main : IO ()
main = repl "average> " showAverage
-}

main : IO ()
main = repl "palindrome?> " $ \s => (show . palindrome $ s) ++ "\n"

