module NeuralNetSigmoid where
-- 伝達関数をシグモイド関数として、すべての入力に対するニューラルネットの出力を求める

import Type

sigmoid :: Double -> Double
sigmoid u = 1 / (1 + exp (-u))

forwardSig :: Input -> Neuron -> Double
forwardSig input (Neuron wn v) =
  let u = sum (zipWith (*) input wn) - v
  in sigmoid u

hiddenSig :: Input -> [Neuron] -> [Double]
hiddenSig input  = map (forwardSig input)

neuralNetSig :: [Input] -> [Neuron] -> Neuron -> [Double]
neuralNetSig inputs hiddenN outN =
  let hiddenRes = map (`hiddenSig` hiddenN) inputs
  in map (`forwardSig` outN) hiddenRes
