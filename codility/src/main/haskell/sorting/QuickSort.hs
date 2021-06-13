{-# LANGUAGE InstanceSigs #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n log n
module QuickSort where

import Data.List (partition)

-----------------------------------------------------------------------------------

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quickSort smallerOrEqual ++ [x] ++ quickSort larger

-----------------------------------------------------------------------------------

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in  quickSort' smallerOrEqual ++ [x] ++ quickSort' larger

-----------------------------------------------------------------------------------

-- using `partition`
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
  where
    (ys, zs) = partition (< x) xs

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input              --> " ++ (show arr)
  print $ "quickSort          --> " ++ (show $ quickSort  arr)
  print $ "quickSort'         --> " ++ (show $ quickSort' arr)
  print $ "qSort w/ partition --> " ++ (show $ qsort arr)
