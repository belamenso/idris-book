import Data.Vect

{- In the game state, you assume that the number of letters still to guess is the same as the length of the vector of missing letters. By putting this in the type of WordState and the type of processGuess, you can be sure that if you ever violate this assumption, your program will no longer compile. -}
data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) ->
                (missing : Vect letters Char) ->
                WordState guesses_remaining letters

data Finished : Type where
  Lost : (game : WordState 0 (S letters)) -> Finished
  Won : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

removeElem : (value : a) -> (xs : Vect (S n) a) ->
                {auto prf : Elem value xs} -> Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = S k} value (y :: ys) {prf = There later} =
  y :: removeElem value ys

nilNotValid : ValidInput [] -> Void
nilNotValid (Letter _) impossible

consNotValid : ValidInput (x :: y :: xs) -> Void
consNotValid (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No nilNotValid
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: y :: xs) = No consNotValid

isValidString : (s : String) -> Dec (ValidInput $ unpack s)
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do
  putStr "Guess> "
  s <- getLine
  case isValidString (toUpper s) of
       No contra => do putStrLn "Bad input"
                       readGuess
       Yes prf => pure (_ ** prf)

{- A value of type WordState guesses letters holds concrete information about system state, including the exact word to be guessed and exactly which letters are still missing. The type itself expresses abstract information about the game state (guesses remaining and number of missing letters), which allows you to express the rules in function types like processGuess. -}
processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) =
  case isElem letter missing of
       Yes prf => Right (MkWordState word (removeElem letter missing))
       No contra => Left (MkWordState word missing)

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do
  (_ ** Letter letter) <- readGuess
  case processGuess letter st of
       Left l => do putStrLn "Wrong!"
                    case guesses of
                         Z => pure (Lost l)
                         S k => game l
       Right r => do putStrLn "Right!"
                     case letters of
                          Z => pure (Won r)
                          S k => game r

main : IO ()
main = do result <- game {guesses=2} (MkWordState "Test" ['T', 'E', 'S'])
          case result of
               Lost (MkWordState word missing) =>
                   putStrLn ("You lose. The word was " ++ word)
               Won game => putStrLn "You win!"

