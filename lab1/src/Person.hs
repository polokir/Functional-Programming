{-# LANGUAGE OverloadedStrings #-}

module Person
    ( 
    getPerson,
    createPerson,
    updatePerson,
    deletePerson
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Person = Person { id :: Int, name :: String, phone :: String, address_id :: Int }
  deriving (Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field

getPerson :: Connection -> Int -> IO [Person]
getPerson conn cid = query conn "SELECT * FROM person WHERE id = ?" (Only cid)

createPerson :: Connection -> String -> String -> Int -> IO [Person]
createPerson conn name phone address_id = do 
  query conn "INSERT INTO person (name, phone, address_id) VALUES (?, ?, ?) RETURNING *" (name, phone, address_id)
  
updatePerson :: Connection -> Int -> String -> String -> Int -> IO [Person]
updatePerson conn cid name phone address_id = do
  query conn "UPDATE person SET name = ?, phone = ?, address_id = ? WHERE id = ? RETURNING *" (name, phone, address_id, cid)
  

deletePerson :: Connection -> Int -> IO Bool
deletePerson conn cid = do
  n <- execute conn "DELETE FROM person WHERE id = ?" (Only cid)
  return $ n > 0
