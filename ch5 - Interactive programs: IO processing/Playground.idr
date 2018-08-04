import System

total readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
     then pure $ Just $ cast input
     else pure Nothing

total readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  Just a <- readNumber | Nothing => pure Nothing
  Just b <- readNumber | Nothing => pure Nothing
  pure $ Just $ (a, b)

total readPair : IO (String, String)
readPair = do a <- getLine
              b <- getLine
              pure (a, b)

total usePair : IO ()
usePair = do (a, b) <- readPair
             putStrLn ("You entered " ++ a ++ " and " ++ b)

total countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off"
countdown (S secs) = do putStrLn . show $ S secs
                        usleep 1000000
                        countdown secs
countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- readNumber
                | Nothing => do putStr "Invalid input\n"
                                countdowns
                countdown startNum
                putStr "Another (y/n)? "
                yn <- getLine
                if yn == "y" then countdowns
                             else pure ()

