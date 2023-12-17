{-# LANGUAGE OverloadedStrings #-}

module Position
    ( 
    getPosition,
    createPosition,
    updatePosition,
    deletePosition
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Position = Position { id :: Int, name :: String, access_level :: Int }
  deriving (Show)

instance FromRow Position where
  fromRow = Position <$> field <*> field <*> field

getPosition :: Connection -> Int -> IO [Position]
getPosition conn cid = query conn "SELECT * FROM position WHERE id = ?" (Only cid)

createPosition :: Connection -> String -> Int -> IO [Position]
createPosition conn name access_level = do 
  query conn "INSERT INTO position (name, access_level) VALUES (?, ?) RETURNING *" (name, access_level)
  
updatePosition :: Connection -> Int -> String -> IO [Position]
updatePosition conn cid name = do
  query conn "UPDATE position SET name = ? WHERE id = ? RETURNING *" (name, cid)
  

deletePosition :: Connection -> Int -> IO Bool
deletePosition conn cid = do
  n <- execute conn "DELETE FROM position WHERE id = ?" (Only cid)
  return $ n > 0
