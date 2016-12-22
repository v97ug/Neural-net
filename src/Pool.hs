module Pool (pooling) where

import Type

pooling :: Matrix' -> Matrix'
pooling [] = []
pooling target =
  let poolSize = 3
  in poolEachLine (take poolSize target) : pooling (drop poolSize target)

poolEachLine :: Matrix' -> [Int]
poolEachLine target
  | null (head target) = []
  | otherwise = maxPooling (map (take pSize) target)
                : poolEachLine (map (drop pSize) target)
  where pSize = length target

maxPooling :: Matrix' -> Int
maxPooling = maximum . concat
