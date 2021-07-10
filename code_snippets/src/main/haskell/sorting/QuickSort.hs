{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n log n
module QuickSort where

import Data.List (partition, sort)

import Text.Printf
import Control.Exception
import System.CPUTime

-----------------------------------------------------------------------------------

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in  quickSort smallerOrEqual ++ [x] ++ quickSort larger

-----------------------------------------------------------------------------------

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quickSort' smallerOrEqual ++ [x] ++ quickSort' larger

-----------------------------------------------------------------------------------

-- using `partition`
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
  where
    (ys, zs) = partition (< x) xs

-----------------------------------------------------------------------------------

timingBigSort :: (Ord a, Show a) => [a] -> IO ()
timingBigSort xs = do
  putStrLn ""
  putStrLn "Sorting big list..."
  let ys = sort xs
  printf "nbrOfElems == %s\n" (show $ length ys)

  start <- getCPUTime
  let zs = quickSort xs
  printf "nbrOfElems quickSorted == %s\n" (show $ length zs)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)

  printf "isSorted == %s\n" (show $ ys == zs)
  printf "Computation time: %0.3f sec\n" (diff :: Double)

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input              --> " ++ (show arr)
  print $ "quickSort          --> " ++ (show $ quickSort  arr)
  print $ "quickSort'         --> " ++ (show $ quickSort' arr)
  print $ "qSort w/ partition --> " ++ (show $ qsort arr)

  let arr2 = ['z','y'..'a']
  print $ "input              --> " ++ (show arr2)
  print $ "quickSort          --> " ++ (show $ quickSort  arr2)

  --
  -- timing measure --> https://chrisdone.com/posts/measuring-duration-in-haskell/
  -- simple timing --> https://wiki.haskell.org/Timing_computations
  -- limiting to 15k as 20k will make my PC hang
  timingBigSort [15000,14999..1]

