/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n²

-- Haskell
deleteFromOri :: Eq a => a -> [a] -> [a]
deleteFromOri _ [] = []
deleteFromOri x (y:ys)
  | x == y = ys
  | otherwise = y:deleteFromOri x ys

selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort xs = mini : selectSort xs'
  where
    mini = minimum xs
    xs' = deleteFromOri mini xs
*/

def selectSort[T <% Ordered[T]](list: List[T]): List[T] = {

  def deleteFromOri(tuple: (T, List[T])): List[T] = {
    tuple match {
      case (_, Nil) => Nil
      case (x, y::ys) =>
        if (x == y) ys
        else y :: deleteFromOri( (x, ys) )
    }
  }

  list match {
    case Nil => Nil
    case xs =>
      val min = xs.min
      val ys = deleteFromOri( (min, xs) )
      min :: selectSort(ys)
  }
}

val arr = (1 to 25).toList.reverse
println(s"input      --> $arr")
println(s"selectSort --> ${selectSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input      --> $arr2")
println(s"selectSort --> ${selectSort(arr2)}")
