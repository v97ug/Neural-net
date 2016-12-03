import Type
import NeuralNetSigmoid

toDoubleArray :: [String] -> [Double]
toDoubleArray = map toDouble
  where
    toDouble :: String -> Double
    toDouble s = read s :: Double

main :: IO ()
main = do
  s <- readFile "input/majority.txt"
  let inputs = (toDoubleArray . words) <$> lines s
      hiddenNeurons = [Neuron [-0.8977349957313048,4.102265004268677,2.102265004268705] (-2.9079878633790495),Neuron [-0.2544493227225049,2.7455506772774902,2.7455506772774902] 1.6623260406730889,Neuron [3.732112288347366,3.732112288347366,3.732112288347366] 5.641957892075575]
      outNeuron = Neuron [-3.1450868177305455,1.7053533587394272,11.628947631918832] 3.818592726675541
  print $ neuralNetSig inputs hiddenNeurons outNeuron
