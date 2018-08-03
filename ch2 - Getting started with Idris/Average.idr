module Average

||| Calculate the average length of words in a string.
||| @str a string containing words separated by whitespace.
export
average : (str : String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum . allLengths . words $ str in
                  cast totalLength / cast numWords
    where
        wordCount : String -> Nat
        wordCount str = length . words $ str

        allLengths : List String -> List Nat
        allLengths strs = map length strs

greet : String -> String
greet "secret" = "handshake"
greet s = s ++ "!"

double : Num ty => ty -> ty
double x = x + x

||| Apply function @f twice
twice : (f : a -> a) -> a -> a
twice f = f . f

f : Int -> Int
f = twice (\s => s * s)

x : Int
x = let a = 10
        b = 20 in
        a + b

