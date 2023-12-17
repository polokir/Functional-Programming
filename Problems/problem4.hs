import Data.List (nub)

type Graph a = [(a, [a])]

-- Функція для знаходження всіх шляхів між двома вершинами у графі
allPaths :: (Eq a) => Graph a -> a -> a -> [[a]]
allPaths graph start end = dfs start end graph []

-- Рекурсивна функція для обходу графа в глибину та знаходження всіх шляхів
dfs :: (Eq a) => a -> a -> Graph a -> [a] -> [[a]]
dfs current end graph path
  | current == end = [path ++ [current]]
  | otherwise =
    let neighbors = case lookup current graph of
                      Just n  -> n
                      Nothing -> []
        validNeighbors = filter (`notElem` path) neighbors
        pathsFromNeighbors = concatMap (\neighbor -> dfs neighbor end graph (path ++ [current])) validNeighbors
    in pathsFromNeighbors

main :: IO ()
main = do
  let exampleGraph = [(1, [2, 3]), (2, [4]), (3, [4]), (4, [5]), (5, [])]
  let startNode = 1
  let endNode = 5

  let paths = allPaths exampleGraph startNode endNode

  putStrLn $ "All paths from " ++ show startNode ++ " to " ++ show endNode ++ ":"
  mapM_ print paths