readToBlank : IO (List String)
readToBlank = do putStrLn "Enter strings and a blank line"
                 helper
where
  helper : IO (List String)
  helper = do
    s <- getLine
    if s == ""
       then pure []
       else do ss <- helper
               pure $ s :: ss

readAndSave : IO ()
readAndSave = do
  lines <- readToBlank
  putStr "Enter a file name: "
  fname <- getLine
  pure ()

