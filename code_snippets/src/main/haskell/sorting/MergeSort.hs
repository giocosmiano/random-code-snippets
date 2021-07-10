{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n log n
module MergeSort where

import Text.Printf
import Control.Exception
import System.CPUTime
import Data.List (sort)

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

timingSort :: (Ord a, Show a) => [a] -> [a] -> IO ()
timingSort xs ys = do
  start <- getCPUTime
  let zs = mergeSort xs
  printf "nbrOfElems mergeSorted == %s\n" (show $ length zs)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "isSorted == %s\n" (show $ ys == zs)
  printf "Computation time: %0.3f sec\n" (diff :: Double)

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

  --
  -- timing measure --> https://chrisdone.com/posts/measuring-duration-in-haskell/
  -- simple timing --> https://wiki.haskell.org/Timing_computations
  putStrLn ""
  putStrLn "Sorting big list..."
  let xs = [1500000,1499999..1]
      ys = sort xs
  printf "nbrOfElems == %s\n" (show $ length ys)
  timingSort xs ys

