import Data.Vect

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
  let text = concat (intersperse "\n" lines) ++ "\n"
  putStr "Enter a file name: "
  fname <- getLine
  Right () <- writeFile fname text
  | Left err => putStrLn ("Error: " ++ show err)
  putStrLn "done"

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right f <- openFile filename Read
  | Left err => do putStrLn "Error opening flie"
                   pure (_ ** [])
  result <- helper f
  closeFile f
  pure result
where
  helper : File -> IO (n ** Vect n String)
  helper f = do
    False <- fEOF f
    | True => pure (_ ** [])
    Right line <- fGetLine f
    | Left err => pure (_ ** [])
    (_ ** lines) <- helper f
    pure (_ ** (line :: lines))

