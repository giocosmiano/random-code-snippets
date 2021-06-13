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

selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort xs = mini : selectSort xs'
  where
    mini = minimum xs
    xs' = deleteFromOri mini xs

-----------------------------------------------------------------------------------

main = do
  let arr = [25,24..1]
  print $ "input      --> " ++ (show arr)
  print $ "selectSort --> " ++ (show $ selectSort  arr)
