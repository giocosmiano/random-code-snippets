{-# LANGUAGE InstanceSigs #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n log n
module MergeSort where

-----------------------------------------------------------------------------------

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge firstHalf secondHalf
  where
    half = length xs `div` 2
    firstHalf  = mergeSort $ take half xs
    secondHalf = mergeSort $ drop half xs

    merge :: (Ord a) => [a] -> [a] -> [a]
    merge x [] = x
    merge [] y = y
    merge allX@(x:xs) allY@(y:ys)
      | x <= y    = x : merge xs allY
      | otherwise = y : merge allX ys

-----------------------------------------------------------------------------------

mergeSort' :: Ord a => [a] -> [a]
mergeSort' [] = []
mergeSort' [x] = [x]
mergeSort' xs =
  merge' (mergeSort' x1) (mergeSort' x2)
    where half = length xs `div` 2
          (x1, x2) = (take half xs, drop half xs)

-----------------------------------------------------------------------------------

mergeSort'' :: Ord a => [a] -> [a]
mergeSort'' [] = []
mergeSort'' [x] = [x]
mergeSort'' xs =
  merge' (mergeSort'' firstHalf) (mergeSort'' secondHalf)
    where half = length xs `div` 2
          firstHalf  = take half xs
          secondHalf = drop half xs

-----------------------------------------------------------------------------------

merge' :: (Ord a) => [a] -> [a] -> [a]
merge' x [] = x
merge' [] y = y
merge' allX@(x:xs) allY@(y:ys)
  | x <= y    = x : merge' xs allY
  | otherwise = y : merge' allX ys

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input       --> " ++ (show arr)
  print $ "mergeSort   --> " ++ (show $ mergeSort   arr)
  print $ "mergeSort'  --> " ++ (show $ mergeSort'  arr)
  print $ "mergeSort'' --> " ++ (show $ mergeSort'' arr)

  let arr2 = ['z','y'..'a']
  print $ "input       --> " ++ (show arr2)
  print $ "mergeSort   --> " ++ (show $ mergeSort  arr2)
