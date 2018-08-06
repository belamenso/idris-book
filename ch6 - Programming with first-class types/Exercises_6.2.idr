import Data.Vect

{- 1 -}
Matrix : Nat -> Nat -> Type
Matrix k j = Vect k $ Vect j Int

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

{- 2 -}
data Format = Number Format -- %d
            | Str Format    -- %s
            | Ch Format     -- %c
            | Flt Format  -- %f
            | Lit String Format -- literal string
            | End           -- and empty format specifier

PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Ch fmt) = Char -> PrintfType fmt
PrintfType (Flt fmt) = Double -> PrintfType fmt
PrintfType (Lit s fmt) = PrintfType fmt
PrintfType End = String

printFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printFmt (Number fmt) acc = \i => printFmt fmt (acc ++ show i)
printFmt (Str fmt) acc = \s => printFmt fmt (acc ++ s)
printFmt (Ch fmt) acc = \c => printFmt fmt (acc ++ strCons c "")
printFmt (Flt fmt) acc = \d => printFmt fmt (acc ++ show d)
printFmt (Lit s fmt) acc = printFmt fmt (acc ++ s)
printFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number $ toFormat chars
toFormat ('%' :: 's' :: chars) = Str $ toFormat chars
toFormat ('%' :: 'c' :: chars) = Ch $ toFormat chars
toFormat ('%' :: 'f' :: chars) = Flt $ toFormat chars
toFormat ('%' :: '%' :: chars) = Lit "%" $ toFormat chars
toFormat (c :: chars)  = case toFormat chars of
                              Lit lit chars' => Lit (strCons c lit) chars'
                              fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat $ unpack fmt)
printf fmt = printFmt _ ""

{- 3 -}
TupleVect : Nat -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, (TupleVect k ty))

test : TupleVect 4 Nat
test = (1,2,3,4,())

