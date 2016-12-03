module Main where

import Lib
import Training
import Type
import System.IO

toDoubleArray :: [String] -> [Double]
toDoubleArray = map toDouble
  where
    toDouble :: String -> Double
    toDouble s = read s :: Double

main :: IO ()
main = do
  s <- readFile "input/majority.txt"
  let inputs = (toDoubleArray . words) <$> lines s
      hiddenNeurons = [Neuron [-2,3,1] (-1), Neuron [-2,1,1] 0.5, Neuron [1,1,1] 0.3]
      outNeuron = Neuron [-3,4,5] (-1)
  print $ training 100 inputs hiddenNeurons outNeuron
