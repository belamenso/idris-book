import Data.Vect

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if x == ""
                 then pure (_ ** [])
                 else do (_ ** xs) <- readVect
                         pure (_ ** x :: xs)

printVect : Show a => (len ** Vect len a) -> IO ()
printVect (len ** xs) =
  putStrLn $ show xs ++ " of length " ++ show len

main : IO ()
main = do
  v <- readVect
  printVect v

