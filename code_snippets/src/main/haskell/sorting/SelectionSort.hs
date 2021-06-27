{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module SelectionSort where

import Text.Printf
import Control.Exception
import System.CPUTime

-----------------------------------------------------------------------------------

deleteFromOri :: Eq a => a -> [a] -> [a]
deleteFromOri _ [] = []
deleteFromOri x (y:ys)
  | x == y = ys
  | otherwise = y:deleteFromOri x ys

-----------------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
  where
    mini = minimum xs
    xs' = deleteFromOri mini xs

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input         --> " ++ (show arr)
  print $ "selectionSort --> " ++ (show $ selectionSort  arr)

  let arr2 = ['z','y'..'a']
  print $ "input         --> " ++ (show arr2)
  print $ "selectionSort --> " ++ (show $ selectionSort  arr2)

  --
  -- timing measure --> https://chrisdone.com/posts/measuring-duration-in-haskell/
  -- simple timing --> https://wiki.haskell.org/Timing_computations
  -- limiting to 15k as 20k will make my PC hang
  let sortedBigArray   = [1..15000]
  let unSortedBigArray = [15000,14999..1]

  start <- getCPUTime
  let isBigArrSorted = selectionSort unSortedBigArray == sortedBigArray
  print $ "isBigArraySorted using `selectionSort` with " ++ show (length unSortedBigArray) ++ " elements --> " ++ (show isBigArrSorted)
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)


