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

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input         --> " ++ (show arr)
  print $ "insertionSort --> " ++ (show $ insertionSort  arr)

  let arr2 = ['z','y'..'a']
  print $ "input         --> " ++ (show arr2)
  print $ "insertionSort --> " ++ (show $ insertionSort  arr2)
