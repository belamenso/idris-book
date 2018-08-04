module Main

ex1 : IO ()
ex1 = do
  a <- getLine
  b <- getLine
  putStrLn . show $ max (length a) (length b)

ex2 : IO ()
ex2 = getLine >>= \a =>
      getLine >>= \b =>
      putStrLn . show $ max (length a) (length b)

