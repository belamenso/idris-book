module Main

import System

total readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
     then pure $ Just $ cast input
     else pure Nothing


guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr $ show guesses ++ " Guess the number> "
  Just num <- readNumber
  | Nothing => do putStrLn "Bad input"
                  guess target guesses
  case compare num target of
       EQ => putStrLn "You won"
       LT => do putStrLn "To few"
                guess target $ guesses + 1
       GT => do putStrLn "To many"
                guess target $ guesses + 1

main : IO ()
main = do t <- time
          guess (fromIntegerNat t) 0

repl' : (prompt : String) -> (f : String -> String) -> IO ()
repl' prompt f = do
  putStr prompt
  s <- getLine
  putStrLn $ f s
  repl' prompt f

repl_with' : (prompt : String) -> (initial_state : a) ->
             (a -> String -> Maybe (String, a)) -> IO ()
repl_with' prompt initial_state f = do
  putStr prompt
  s <- getLine
  let Just (new_str, new_state) = f initial_state s
    | Nothing => pure ()
  putStrLn new_str
  repl_with' prompt new_state f

