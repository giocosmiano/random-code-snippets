{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module SelectionSort where

import Text.Printf
import Control.Exception
import System.CPUTime
import Data.List (sort)

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

timingSort :: (Ord a, Show a) => [a] -> [a] -> IO ()
timingSort xs ys = do
  start <- getCPUTime
  let zs = selectionSort xs
  printf "nbrOfElems selectionSorted == %s\n" (show $ length zs)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "isSorted == %s\n" (show $ ys == zs)
  printf "Computation time: %0.3f sec\n" (diff :: Double)

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
  putStrLn ""
  putStrLn "Sorting big list..."
  let xs = [15000,14999..1]
      ys = sort xs
  printf "nbrOfElems == %s\n" (show $ length ys)
  timingSort xs ys


