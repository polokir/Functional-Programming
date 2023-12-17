import System.Random
import Data.List (permutations, minimumBy, foldl')
import Data.Function (on)

-- Тип для представлення міст у задачі комівояджера
type City = (Int, Int)

-- Тип для представлення особини (шляху) у генетичному алгоритмі
type Individual = [City]

-- Тип для представлення популяції особин
type Population = [Individual]

-- Тип для представлення конфігурації генетичного алгоритму
data GAConfig = GAConfig
  { populationSize :: Int
  , generations :: Int
  , crossoverRate :: Double
  , mutationRate :: Double
  }

-- Функція для обчислення відстані між двома містами
distance :: City -> City -> Double
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

-- Функція для обчислення відстані у маршруті
routeDistance :: [City] -> Double
routeDistance route = sum $ zipWith distance route (tail route ++ [head route])

-- Функція для створення випадкової особини
randomIndividual :: [City] -> IO Individual
randomIndividual cities = do
  shuffled <- shuffle cities
  return shuffled

-- Функція для створення випадкової популяції
randomPopulation :: [City] -> Int -> IO Population
randomPopulation cities size = sequence $ replicate size (randomIndividual cities)

-- Функція для перемішування списку випадковим чином
shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newStdGen
  return $ fst $ unzip $ take (length xs) $ randomPerm gen xs
  where
    randomPerm g [] = []
    randomPerm g xs = let (n, newGen) = randomR (0, length xs - 1) g
                          front = xs !! n
                          back = take n xs ++ drop (n + 1) xs
                      in (front, newGen) : randomPerm newGen back

-- Функція для обчислення придатності (відстані) для кожної особини
fitness :: Population -> [(Individual, Double)]
fitness population = map (\ind -> (ind, routeDistance ind)) population

selectParents :: Population -> IO (Individual, Individual)
selectParents population = do
  parent1 <- selectParent population
  parent2 <- selectParent population
  return (parent1, parent2)

selectParent :: Population -> IO Individual
selectParent population = do
  let totalFitness = sum $ map snd (fitness population)
  let roulette = scanl1 (\(_, acc) (ind, fit) -> (ind, acc + fit / totalFitness)) (fitness population)
  r <- randomRIO (0, 1)
  return $ fst $ head $ dropWhile (\(_, acc) -> acc < r) roulette

crossover :: Individual -> Individual -> IO Individual
crossover parent1 parent2 = do
  point <- randomRIO (0, length parent1 - 1)
  let (start, end) = if point == 0 then (0, length parent1) else (0, point)
  let slicedParent1 = take point parent1 ++ drop point parent2
  let slicedParent2 = take point parent2 ++ drop point parent1
  return $ repairRoute $ if point == 0 then slicedParent1 else slicedParent2
  where
    repairRoute route = nubBy (\x y -> fst x == fst y) route

mutate :: Individual -> IO Individual
mutate route = do
  idx1 <- randomRIO (0, length route - 1)
  idx2 <- randomRIO (0, length route - 1)
  let mutatedRoute = swapElements idx1 idx2 route
  return $ repairRoute mutatedRoute
  where
    swapElements i j xs =
      let ith = xs !! i
          jth = xs !! j
          left = take i xs ++ [jth] ++ drop (i + 1) xs
          right = take j xs ++ [ith] ++ drop (j + 1) xs
      in left ++ [ith] ++ drop (i + 1) right
    repairRoute route = nubBy (\x y -> fst x == fst y) route

geneticAlgorithm :: [City] -> GAConfig -> IO Individual
geneticAlgorithm cities config = do
  initialPopulation <- randomPopulation cities (populationSize config)
  finalPopulation <- evolve config initialPopulation (generations config)
  let bestRoute = fst $ minimumBy (compare `on` snd) (fitness finalPopulation)
  return bestRoute

evolve :: GAConfig -> Population -> Int -> IO Population
evolve config population 0 = return population
evolve config population n = do
  newPopulation <- evolveStep config population
  evolve config newPopulation (n - 1)

evolveStep :: GAConfig -> Population -> IO Population
evolveStep config population = do
  newGeneration <- replicateM (populationSize config `div` 2) $ do
    (parent1, parent2) <- selectParents population
    child1 <- crossover parent1 parent2
    child2 <- crossover parent2 parent1
    mutateRate1 <- randomRIO (0.0, 1.0)
    mutateRate2 <- randomRIO (0.0, 1.0)
    mutatedChild1 <- if mutateRate1 < mutationRate config then mutate child1 else return child1
    mutatedChild2 <- if mutateRate2 < mutationRate config then mutate child2 else return child2
    return mutatedChild1 ++ return mutatedChild2
  return $ population ++ concat newGeneration

main :: IO ()
main = do
  let cities = [(0, 0), (1, 2), (3, 1), (5, 2), (6, 0)]
  let config = GAConfig
        { populationSize = 100
        , generations = 1000
        , crossoverRate = 0.8
        , mutationRate = 0.2
        }
  
  result <- geneticAlgorithm cities config
  putStrLn "Best Route:"
  print result
  putStrLn $ "Total Distance: " ++ show (routeDistance result)