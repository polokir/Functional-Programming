type Position = (Int, Int)
type Board = [[Int]]

boardSize :: Int
boardSize = 8

startPosition :: Position
startPosition = (0, 0)


isValid :: Position -> Bool
isValid (x, y) = x >= 0 && y >= 0 && x < boardSize && y < boardSize

isNotVisited :: Board -> Position -> Bool
isNotVisited board (x, y) = board !! x !! y == 0


possibleMoves :: Board -> Position -> [Position]
possibleMoves board (x, y) =
  filter (\(a, b) -> isValid (a, b) && isNotVisited board (a, b)) moves
  where
    moves = [ (x + 2, y + 1), (x + 1, y + 2), (x - 1, y + 2), (x - 2, y + 1)
            , (x - 2, y - 1), (x - 1, y - 2), (x + 1, y - 2), (x + 2, y - 1)
            ]


knightTour :: Board -> Position -> Int -> Maybe Board
knightTour board pos@(x, y) count
  | count == boardSize * boardSize = Just board
  | otherwise =
    case filter (\move -> knightTour (updateBoard board move count) move (count + 1) /= Nothing) (possibleMoves board pos) of
      [] -> Nothing
      (nextMove:_) -> knightTour (updateBoard board nextMove count) nextMove (count + 1)


updateBoard :: Board -> Position -> Int -> Board
updateBoard board (x, y) count =
  take x board ++ [take y (board !! x) ++ [count] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board


findKnightTour :: Maybe Board
findKnightTour = knightTour (replicate boardSize (replicate boardSize 0)) startPosition 1


main :: IO ()
main =
  case findKnightTour of
    Just solution -> mapM_ print solution
    Nothing -> putStrLn "Розв'язок не знайдено."
