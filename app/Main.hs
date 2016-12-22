module Main where

import Lib
import Training
import TrainingPict
import Type
import NeuralNetSigmoid
import System.IO
import Conv
import Pool

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

-- 多数決の学習をし、結果を表示する
testMajority :: IO ()
testMajority = do
  s <- readFile "input/majority.txt"
  let inputs = (toDoubleArray . words) <$> lines s
      hiddenNeurons = [Neuron [-2,3,1] (-1), Neuron [-2,1,1] 0.5, Neuron [1,1,1] 0.3]
      outNeuron = Neuron [-3,4,5] (-1)
  -- print $ training 100 inputs hiddenNeurons outNeuron
  let (errors, _, _) = trainingProcess [100] inputs hiddenNeurons outNeuron
  print $ zip [1..] errors

toInt' :: String -> Int
toInt' s = read s :: Int

strToIntArr :: String -> [[Int]]
strToIntArr imageTxt =
  let sList = words <$> lines imageTxt
  in map (map toInt') sList

main :: IO ()
main = do
  -- testMajority
  -- testXOR
  let imageFileNames = ["input/image1.txt", "input/image2.txt", "input/image3.txt"]
  s <- sequence $ fmap readFile imageFileNames

  let imgData = map strToIntArr s :: [[[Int]]]
      filt = [[0,1,0], [0,1,0], [0,1,0]]
      convOut = map (conv' filt) imgData :: [[[Int]]]
      poolOut = map pooling convOut
  print poolOut
