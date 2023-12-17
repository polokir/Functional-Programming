import System.Random
import Data.List (transpose)

type Layer = [Neuron]

data Neuron = Neuron
  { weights :: [Double]  
  , bias :: Double       
  }

data NeuralNetwork = NeuralNetwork
  { layers :: [Layer]    -- Шари персептрону
  }

initializeNeuron :: Int -> IO Neuron
initializeNeuron inputSize = do
  gen <- newStdGen
  let weights = take inputSize $ randomRs (-1.0, 1.0) gen
  let bias = head $ randomRs (-1.0, 1.0) gen
  return (Neuron weights bias)

initializeLayer :: Int -> Int -> IO Layer
initializeLayer inputSize numNeurons =
  sequence $ replicate numNeurons (initializeNeuron inputSize)

initializeNeuralNetwork :: [Int] -> IO NeuralNetwork
initializeNeuralNetwork layerSizes = do
  let numLayers = length layerSizes
  let inputSizes = take (numLayers - 1) layerSizes
  let outputSizes = drop 1 layerSizes
  layers <- zipWithM initializeLayer inputSizes outputSizes
  return (NeuralNetwork layers)

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoidDerivative :: Double -> Double
sigmoidDerivative x = x * (1 - x)

feedForward :: Layer -> [Double] -> [Double]
feedForward layer inputs = map (\neuron -> sigmoid (sum (zipWith (*) (weights neuron) inputs) + bias neuron)) layer

backpropagate :: Layer -> [Double] -> [Double] -> (Layer, [Double])
backpropagate layer inputs errors = (newLayer, newErrors)
  where
    output = map (\neuron -> sigmoid (sum (zipWith (*) (weights neuron) inputs) + bias neuron)) layer
    newErrors = zipWith (*) (map sigmoidDerivative output) errors
    newLayer = zipWith updateNeuron layer newErrors
    updateNeuron neuron err = Neuron
      { weights = zipWith (\w e -> w - learningRate * e * head inputs) (weights neuron) newErrors
      , bias = bias neuron - learningRate * head newErrors
      }

-- Тренування персептрону
train :: NeuralNetwork -> [[Double]] -> [[Double]] -> Int -> NeuralNetwork
train network inputs targets epochs =
  foldl (\nn _ -> trainEpoch nn inputs targets) network [1..epochs]

-- Тренування персептрону для однієї епохи
trainEpoch :: NeuralNetwork -> [[Double]] -> [[Double]] -> NeuralNetwork
trainEpoch network inputs targets =
  foldl (\nn (input, target) -> trainExample nn input target) network (zip inputs targets)

-- Тренування персептрону для одного прикладу
trainExample :: NeuralNetwork -> [Double] -> [Double] -> NeuralNetwork
trainExample network input target =
  NeuralNetwork (zipWith3 backpropagate newLayers inputs errors)
  where
    inputs = input : feedForwardInputs
    feedForwardInputs = map (flip feedForward input) (init (layers network))
    errors = zipWith (\output target -> target - output) (last feedForwardInputs) target
    newLayers = layers network

-- Параметри навчання
learningRate :: Double
learningRate = 0.1

main :: IO ()
main = do
  let inputSize = 2
  let hiddenSize = 3
  let outputSize = 1
  let layerSizes = [inputSize, hiddenSize, outputSize]

  network <- initializeNeuralNetwork layerSizes

  let inputs = [[0, 0], [0, 1], [1, 0], [1, 1]]
  let targets = [[0], [1], [1], [0]]

  let epochs = 10000
  let trainedNetwork = train network inputs targets epochs

  putStrLn "Trained Network Predictions:"
  mapM_ (print . head . head . feedForward (last (layers trainedNetwork))) inputs