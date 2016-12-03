module NeuralNetStep where
-- 伝達関数をステップ関数として、すべての入力に対するニューラルネットの出力を求める

import Type

step :: Double -> Double
step u = if u >= 0 then 1 else 0

forward :: Input -> Neuron -> Double
forward input (Neuron wn v) =
  let u = sum (zipWith (*) input wn) - v
  in step u

hidden :: Input -> [Neuron] -> [Double]
hidden input  = map (forward input)

neuralNet :: [Input] -> [Neuron] -> Neuron -> [Double]
neuralNet inputs hiddenN outN =
  let hiddenRes = map (`hidden` hiddenN) inputs
  in map (`forward` outN) hiddenRes
