{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module InsertionSort where

import Text.Printf
import Control.Exception
import System.CPUTime
import Data.List (sort)

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

timingBigSort :: (Ord a, Show a) => [a] -> IO ()
timingBigSort xs = do
  putStrLn ""
  putStrLn "Sorting big list..."
  let ys = sort xs
  printf "nbrOfElems == %s\n" (show $ length ys)

  start <- getCPUTime
  let zs = insertionSort xs
  printf "nbrOfElems insertionSorted == %s\n" (show $ length zs)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)

  printf "isSorted == %s\n" (show $ ys == zs)
  printf "Computation time: %0.3f sec\n" (diff :: Double)

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input         --> " ++ (show arr)
  print $ "insertionSort --> " ++ (show $ insertionSort  arr)

  let arr2 = ['z','y'..'a']
  print $ "input         --> " ++ (show arr2)
  print $ "insertionSort --> " ++ (show $ insertionSort  arr2)

  --
  -- timing measure --> https://chrisdone.com/posts/measuring-duration-in-haskell/
  -- simple timing --> https://wiki.haskell.org/Timing_computations
  -- limiting to 15k as 20k will make my PC hang
  timingBigSort [15000,14999..1]


