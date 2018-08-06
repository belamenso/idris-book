module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (ds : DataStore) ->
                   SchemaType (schema ds) ->
                   DataStore
addToStore (MkData schema size items) newitem =
  MkData schema _ (items ++ [newitem])

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display a

getEntry : (pos : Integer) -> (store : DataStore) ->
                 Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
      case integerToFin pos (size store) of
           Nothing => Just ("Out of range\n", store)
           Just id => Just (display (index id store_items) ++ "\n",
                            store)

data Command : Schema -> Type where
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema

parseBySchema : (schema : Schema) -> String
                -> Maybe (SchemaType schema)
parseBySchema schema x = ?parseBySchema_rhs
   
parseCommand : (schema : Schema) -> String -> String ->
                     Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restOk => Just $ Add restOk
parseCommand schema "get" val = case all isDigit $ unpack val of
                                     False => Nothing
                                     True => Just $ Get $ cast val
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) ->
        (input : String) ->
        Maybe $ Command schema
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse (schema store) inp of
       Nothing => Just ("Invalid command\n", store)
       Just (Add item) =>
         Just ("ID " ++ show (size store) ++ "\n",
               addToStore store item)
       Just (Get pos) => getEntry pos store
       Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ [])
                "Command> " processInput

