{-# LANGUAGE OverloadedStrings #-}

module Address
    (
    getAddress,
    createAddress,
    updateAddress,
    deleteAddress
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Address = Address { 
    id :: Int, 
    street :: String,
    city :: String
} deriving (Show)

instance FromRow Address where
  fromRow = Address <$> field <*> field <*> field

getAddress :: Connection -> Int -> IO [Address]
getAddress conn cid = query conn "SELECT * FROM address WHERE id = ?" (Only cid)

createAddress :: Connection -> String -> String -> IO [Address]
createAddress conn street city = do
  query conn "INSERT INTO address (street, city) VALUES (?, ?) RETURNING *" (street, city)

updateAddress :: Connection -> Int -> String -> String -> IO [Address]
updateAddress conn cid street city = do
  query conn "UPDATE address SET street = ?, city = ? WHERE id = ? RETURNING *" (street, city, cid)


deleteAddress :: Connection -> Int -> IO Bool
deleteAddress conn cid = do
  n <- execute conn "DELETE FROM address WHERE id = ?" (Only cid)
  return $ n > 0
