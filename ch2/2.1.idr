module Main

average : String -> Double
average str = let numWords = wordCount str
                  totalLength = sum . allLengths . words $ str in
                  cast totalLength / cast numWords
    where
        wordCount : String -> Nat
        wordCount str = length . words $ str

        allLengths : List String -> List Nat
        allLengths strs = map length strs

showAverage : String -> String
showAverage str = "The average word length is: " ++
                  show (average str) ++ "\n"

greet : String -> String
greet "secret" = "handshake"
greet s = s ++ "!"

double : Num ty => ty -> ty
double x = x + x

twice : (a -> a) -> a -> a
twice f = f . f

f : Int -> Int
f = twice (\s => s * s)

x : Int
x = let a = 10
        b = 20 in
        a + b

main : IO ()
main = repl "Enter a string: " showAverage

