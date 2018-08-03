module Main

StringOrInt : Bool -> Type
StringOrInt x = case x of
                     True => Int
                     False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of True => 94
                             False => "ninety four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of
                         True => ?trueVal -- cast val
                         False => ?falseVal -- val

square : Num a => a -> a
square x = x * x

main : IO ()
main = do
    putStrLn (valToString False "ddddd")
    putStrLn (valToString True 12)

