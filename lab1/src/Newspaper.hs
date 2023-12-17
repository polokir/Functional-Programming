{-# LANGUAGE OverloadedStrings #-}

module Newspaper
    ( 
    getNewspaper,
    createNewspaper,
    updateNewspaper,
    deleteNewspaper
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Newspaper = Newspaper { id :: Int, about :: String, staff_id :: Int }
  deriving (Show)

instance FromRow Newspaper where
  fromRow = Newspaper <$> field <*> field <*> field

getNewspaper :: Connection -> Int -> IO [Newspaper]
getNewspaper conn cid = query conn "SELECT * FROM newspaper WHERE id = ?" (Only cid)

createNewspaper :: Connection -> String -> Int -> IO [Newspaper]
createNewspaper conn about staff_id = do 
  query conn "INSERT INTO newspaper (about, staff_id) VALUES (?, ?) RETURNING *" (about, staff_id)
  
updateNewspaper :: Connection -> Int -> String -> Int -> IO [Newspaper]
updateNewspaper conn cid about staff_id = do
  query conn "UPDATE newspaper SET about = ?, staff_id = ? WHERE id = ? RETURNING *" (about, staff_id, cid)
  

deleteNewspaper :: Connection -> Int -> IO Bool
deleteNewspaper conn cid = do
  n <- execute conn "DELETE FROM newspaper WHERE id = ?" (Only cid)
  return $ n > 0
