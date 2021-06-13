{-# LANGUAGE InstanceSigs #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module BubbleSort where

-----------------------------------------------------------------------------------

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
  | x1 > x2 = x2 : swaps(x1:xs)
  | otherwise = x1 : swaps(x2:xs)

-----------------------------------------------------------------------------------

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs
  | swaps xs == xs = xs -- did not change, stop
  | otherwise = bubbleSort $ swaps xs

-----------------------------------------------------------------------------------

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' [] = []
bubbleSort' xs = bubbleSort' initElem ++ [lastElem]
  where
    swappedElem = swaps xs
    initElem = init swappedElem
    lastElem = last swappedElem

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input       --> " ++ (show arr)
  print $ "bubbleSort  --> " ++ (show $ bubbleSort  arr)
  print $ "bubbleSort' --> " ++ (show $ bubbleSort' arr)
