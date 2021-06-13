{-# LANGUAGE InstanceSigs #-}

-- https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
-- time complexity --> n²
module SelectionSort where

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
