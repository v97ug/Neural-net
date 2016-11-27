module Main where

import Lib
import System.IO
import Debug.Trace

type Input = [Double]
data Neuron = Neuron [Double] Double deriving (Show)

toDouble :: String -> Double
toDouble s = read s :: Double

toDoubleArray :: [String] -> [Double]
toDoubleArray = map toDouble

forwards :: [Input] -> Neuron -> [Double]
forwards inputs neuron =
  map (`forward` neuron) inputs

forward :: Input -> Neuron -> Double
forward input (Neuron wn v) =
  let u = sum (zipWith (*) input wn) - v
  in step u

forwardSig :: Input -> Neuron -> Double
forwardSig input (Neuron wn v) =
  let u = sum (zipWith (*) input wn) - v
  in sigmoid u

step :: Double -> Double
step u = if u >= 0 then 1 else 0

sigmoid :: Double -> Double
sigmoid u = 1 / (1 + exp (-u))

hidden :: Input -> [Neuron] -> [Double]
hidden input  = map (forward input)

neuralNet :: [Input] -> [Neuron] -> Neuron -> [Double]
neuralNet inputs hiddenN outN =
  let hiddenRes = map (`hidden` hiddenN) inputs
  in map (`forward` outN) hiddenRes

alpha = 10

--出力層の重みとしきい値の調整
oLearn :: Neuron -> [Double] -> Input -> Double -> Neuron
oLearn (Neuron wn v) hiddenOut input output =
  let d = (last input - output) * output * (1 - output)
      newWn = zipWith (+) wn $ map ((* alpha).(* d)) hiddenOut
      newV = v + alpha * (-1.0) * d
  in Neuron newWn newV

--中間層の重みとしきい値の調整
hLearn :: [Neuron] -> [Double] -> Neuron -> Input -> Double -> [Neuron]
hLearn hiddenNs hiddenOut (Neuron wo vo) input o =
  let djList = zipWith (\wn hOut1 -> hOut1 * (1-hOut1) * wn * (last input - o) * o * (1-o)) wo hiddenOut
      fixWeight = zipWith (\input dj -> alpha * input * dj) input djList
      fixTh = map (\dj -> alpha * (-1.0) * dj) djList
      newWn = zipWith (\(Neuron wh _) fixW -> map (+fixW) wh) hiddenNs fixWeight
      newV = zipWith (\(Neuron _ vh) fixT -> vh + fixT) hiddenNs fixTh
  in zipWith Neuron newWn newV

backPropagation :: Double -> [Input] -> [Neuron] -> Neuron -> (Double, [Neuron], Neuron)
backPropagation err [] hiddenN outN = (err, hiddenN, outN)
backPropagation err (x:xs) hiddenN outN =
  let hiddenOut = map (forwardSig x) hiddenN -- 中間層の出力
      o = forwardSig hiddenOut outN
      newOutN = oLearn outN hiddenOut x o
      newHiddenN = hLearn hiddenN hiddenOut newOutN x o
      newErr = err + (o - last x) ^ 2
  in backPropagation newErr xs newHiddenN newOutN

training :: Double -> [Input] -> [Neuron] -> Neuron -> (Double, [Neuron], Neuron)
training err inputs hiddenN outN
  | err <= limit = (err, hiddenN, outN)
  | otherwise =  trace (show err) $ training err' inputs hiddenN' outN'
  where
    limit = 0.001
    (err', hiddenN', outN') = backPropagation 0 inputs hiddenN outN


main :: IO ()
main = do
  -- s <- readFile "input/data.txt"
  s <- readFile "input/majority.txt"
  let inputs = (toDoubleArray . words) <$> lines s
      -- neuronAND = Neuron [1,1] 1.5
      hiddenNeurons = [Neuron [-2,3,1] (-1), Neuron [-2,1,1] 0.5]
      outNeuron = Neuron [-60,94,50] (-1)
  -- print $ forwards inputs (Neuron [1,1] 1.5)
  -- print $ neuralNet inputs hiddenNeurons outNeuron
  print $ training 100 inputs hiddenNeurons outNeuron
