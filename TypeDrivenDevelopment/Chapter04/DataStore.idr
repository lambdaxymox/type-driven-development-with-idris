module Main

import Data.Vect


data DataStore : Type where
  MkData : (size : Nat)
        -> (items : Vect size String)
        -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | -- Exercise 1
               Size
             | -- Exercise 2
               Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just . Get . cast $ val
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just . Search $ str
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
  = let storeItems = items store in
        case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          Just id => Just (index id storeItems ++ "\n", store)

-- Exercise 2 and Exercise 3
searchString : (store : DataStore) -> (str : String) -> String
searchString store str = searchString' 0 (items store) str
  where
    searchString' : Nat -> Vect n String -> String -> String
    searchString' idx [] str = ""
    searchString' idx (x :: xs) str
      = let remainder = searchString' (idx + 1) xs str
        in case str `isInfixOf` x of
             False => remainder
             True => "ID " ++ show idx ++ ": " ++ x ++ "\n" ++ remainder

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
        Nothing => Just ("Invalid command\n", store)
        Just (Add item) =>
          Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
        Just (Get pos) => getEntry pos store
        Just Size =>
          Just ("Total number of entries: " ++ show (size store) ++ "\n", store)
        Just (Search str) => Just (searchString store str, store)
        Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
