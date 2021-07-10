{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module BubbleSort where

import Text.Printf
import Control.Exception
import System.CPUTime
import Data.List (sort)

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

timingSort :: (Ord a, Show a) => [a] -> [a] -> IO ()
timingSort xs ys = do
  start <- getCPUTime
  let zs = bubbleSort xs
  printf "nbrOfElems bubbleSorted == %s\n" (show $ length zs)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "isSorted == %s\n" (show $ ys == zs)
  printf "Computation time: %0.3f sec\n" (diff :: Double)

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input       --> " ++ (show arr)
  print $ "bubbleSort  --> " ++ (show $ bubbleSort  arr)
  print $ "bubbleSort' --> " ++ (show $ bubbleSort' arr)

  let arr2 = ['z','y'..'a']
  print $ "input       --> " ++ (show arr2)
  print $ "bubbleSort  --> " ++ (show $ bubbleSort  arr2)

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

