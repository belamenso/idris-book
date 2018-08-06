%default total

data Format = Number Format -- %d + rest
            | Str Format    -- %s + rest
            | Lit String Format -- literal string + rest
            | End           -- and empty format specifier
-- clean separation between parsing and processing
-- Str (Lit " = " (Number End)) -- "%s = %d"

-- intermediate types are helpfun just like intermediate functions
-- are helpful, they give us more editing features (case splitting)
PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Lit s fmt) = PrintfType fmt
PrintfType End = String

printFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printFmt (Number fmt) acc = \i => printFmt fmt (acc ++ show i)
printFmt (Str fmt) acc = \s => printFmt fmt (acc ++ s)
printFmt (Lit s fmt) acc = printFmt fmt (acc ++ s)
printFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number $ toFormat chars
toFormat ('%' :: 's' :: chars) = Str $ toFormat chars
toFormat ('%' :: '%' :: chars) = Lit "%" $ toFormat chars
toFormat (c :: chars)  = case toFormat chars of
                              Lit lit chars' => Lit (strCons c lit) chars'
                              fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat $ unpack fmt)
printf fmt = printFmt _ ""

