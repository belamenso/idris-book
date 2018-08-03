module Palindrome

export
palindrome : String -> Bool
palindrome str = str == reverse str

ic_palindrome : String -> Bool
ic_palindrome = palindrome . toLower

nat_palindrome : Nat -> String -> Bool
nat_palindrome k str = length str > k && palindrome str

ten_palindrome : String -> Bool
ten_palindrome = nat_palindrome 10

export
counts : String -> (Nat, Nat)
counts str = (wordCount, chars)
where wordCount = length . words $ str
      chars = length str

top_ten : Ord a => List a -> List a
top_ten xs = take 10 ( reverse ( sort xs))

over_length : Nat -> List String -> Nat
over_length k = length . filter (\s => length s > k)

