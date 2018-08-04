module Main

import Data.Vect

-- DATA
data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

total size : DataStore -> Nat
size (MkData size items) = size

total items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

total addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem =
  MkData _ (items ++ [newitem])

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

-- PARSING

total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just $ Add str
parseCommand "get" val = case all isDigit $ unpack val of
                              False => Nothing
                              True => Just $ Get $ cast val
parseCommand "search" str = Just $ Search str
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand _ _ = Nothing

total parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

-- OPERATION

total getEntry : (pos : Integer) -> (store : DataStore) ->
                 Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
      case integerToFin pos (size store) of
           Nothing => Just ("Out of range\n", store)
           Just id => Just (index id store_items ++ "\n", store)

-- this is quite ugly because Idris needs immediate types for it to work
total getSearchResults : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
getSearchResults str (MkData size items) =
  let presentableString = (the String $ concat $ intersperse "\n" (map joinPair results)) ++ "\n"
      in Just (presentableString, MkData size items)
      where
        list_items : List String
        list_items = toList items

        items_with_nums : List (Nat, String)
        items_with_nums = zipWith (\a, b => (a, b)) [0..(length list_items)] list_items

        results : List (Nat, String)
        results = filter (\(n, s) => isInfixOf str s) items_with_nums

        joinPair : (Nat, String) -> String
        joinPair (n, s) = show n ++ ". " ++ s

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
       Nothing => Just ("Invalid command\n", store)
       Just (Add item) => Just ("ID " ++ show (size store) ++ "\n",
                                addToStore store item)
       Just (Search str) => getSearchResults str store
       Just (Get pos) => getEntry pos store
       Just Size => Just (show (size store) ++ "\n", store)
       Just Quit => Nothing

-- REPL

main : IO ()
main = replWith (MkData _ []) "Command> " processInput

