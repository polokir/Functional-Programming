module Main (main) where

import Position
import Address
import Staff
import Person
import Newspaper
import Database.PostgreSQL.Simple

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectDatabase = "postgres"
        , connectUser = "postgres"
        , connectPassword = "postgres"
        }

main :: IO ()
main = do
  conn <- connect localPG
  putStrLn "Name of your position? "
  positionName <- getLine
  newPosition <- createPosition conn positionName 5
  putStrLn $ "New Position: " ++ show newPosition
  firstPositions <- getPosition conn 1
  putStrLn $ "First Position: " ++ show firstPositions
  isDeleted <- deletePosition conn 1
  putStrLn $ "First Position deleted: " ++ show isDeleted
  firstPositions <- getPosition conn 1
  putStrLn $ "First Position: " ++ show firstPositions
  secondPositions <- getPosition conn 2
  putStrLn $ "Second Position: " ++ show secondPositions
  updatedSecond <- updatePosition conn 2 "NewPosition"
  putStrLn $ "Updated Position: " ++ show updatedSecond
  secondPositions <- getPosition conn 2
  putStrLn $ "Second Position: " ++ show secondPositions
  