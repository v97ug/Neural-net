module Conv (conv') where

import Type

conv' :: Matrix' -> Matrix' -> Matrix'
conv' filt all@(x:xs)
  | length all < length filt = []
  | otherwise = eachLine filt (take 3 all) : conv' filt xs

eachLine :: Matrix' -> Matrix' -> [Int]
eachLine filt target
  | length (head target) < length filt = []
  | otherwise = calConv filt (map (take 3) target)
                : eachLine filt (map (drop 1) target)

calConv :: Matrix' -> Matrix' -> Int
calConv filt target = sum $ zipWith (*) (concat filt) (concat target)
