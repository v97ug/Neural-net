module Main where

import Lib
import Training
import Type
import NeuralNetSigmoid
import System.IO

toDoubleArray :: [String] -> [Double]
toDoubleArray = map toDouble
  where
    toDouble :: String -> Double
    toDouble s = read s :: Double

-- XORの学習をし、結果を表示する
testXOR :: IO ()
testXOR = do
  s <- readFile "input/xor.txt"
  let inputs = (toDoubleArray . words) <$> lines s
      hiddenNeurons = [Neuron [-2,3] (-1), Neuron [-2,1] 0.5]
      outNeuron = Neuron [-3,4] (-1)
      (_, resHiddenN, resOutN) = training 100 inputs hiddenNeurons outNeuron
      results = neuralNetSig inputs resHiddenN resOutN
  mapM_ print $ zip (map init inputs) results

main :: IO ()
main = do
  s <- readFile "input/majority.txt"
  let inputs = (toDoubleArray . words) <$> lines s
      hiddenNeurons = [Neuron [-2,3,1] (-1), Neuron [-2,1,1] 0.5, Neuron [1,1,1] 0.3]
      outNeuron = Neuron [-3,4,5] (-1)
  print $ training 100 inputs hiddenNeurons outNeuron
  -- testXOR
