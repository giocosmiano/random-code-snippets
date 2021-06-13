{-# LANGUAGE InstanceSigs #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module InsertionSort where

-----------------------------------------------------------------------------------

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  |  x < y = x:y:ys
  | otherwise = y : insert x ys

-----------------------------------------------------------------------------------

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input      --> " ++ (show arr)
  print $ "insertSort --> " ++ (show $ insertSort  arr)
