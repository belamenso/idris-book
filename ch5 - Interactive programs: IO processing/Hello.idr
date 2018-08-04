module Main

printLength : IO ()
printLength =
  getLine >>=
  putStrLn . show . length

doExample : IO ()
doExample = do
  input <- getLine
  let len = length input
  putStrLn $ show len

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn $ "Hello " ++ x ++ "!"

