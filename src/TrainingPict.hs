module TrainingPict where

import Training
import Type

trainingProcess :: [Double] -> [Input] -> [Neuron] -> Neuron -> ([Double], [Neuron], Neuron)
trainingProcess errors inputs hiddenN outN
  | err' <= limit = (errors ++ [err'], hiddenN, outN)
  | otherwise = trainingProcess (errors ++ [err']) inputs hiddenN' outN'
  where
    limit = 0.001
    (err', hiddenN', outN') = backPropagation 0 inputs hiddenN outN
