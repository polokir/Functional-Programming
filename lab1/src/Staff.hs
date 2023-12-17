{-# LANGUAGE OverloadedStrings #-}

module Staff
    ( 
    getStaff,
    createStaff,
    updateStaff,
    deleteStaff
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Staff = Staff { id :: Int, salary :: Int, person_id :: Int, position_id :: Int }
  deriving (Show)

instance FromRow Staff where
  fromRow = Staff <$> field <*> field <*> field <*> field 

getStaff :: Connection -> Int -> IO [Staff]
getStaff conn cid = query conn "SELECT * FROM staff WHERE id = ?" (Only cid)

createStaff :: Connection -> Int -> Int -> Int -> IO [Staff]
createStaff conn salary person_id position_id = do 
  query conn "INSERT INTO staff (salary, person_id, position_id) VALUES (?, ?, ?) RETURNING *" (salary, person_id, position_id)
  
updateStaff :: Connection -> Int -> Int -> Int -> Int -> IO [Staff]
updateStaff conn cid salary person_id position_id = do
  query conn "UPDATE staff SET salary = ?, person_id = ?, position_id = ? WHERE id = ? RETURNING *" (salary, person_id, position_id, cid)
  

deleteStaff :: Connection -> Int -> IO Bool
deleteStaff conn cid = do
  n <- execute conn "DELETE FROM staff WHERE id = ?" (Only cid)
  return $ n > 0
