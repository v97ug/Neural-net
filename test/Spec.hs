main :: IO ()
main = do
  s <- readFile "input/data.txt"
  putStrLn s
