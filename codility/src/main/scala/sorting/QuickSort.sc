/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n log n

-- Haskell
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in  quickSort smallerOrEqual ++ [x] ++ quickSort larger
*/

def quickSort[T](list: List[T])(implicit ev$1: T => Ordered[T]): List[T] = {
  list match {
    case Nil => Nil
    case x::xs =>
      List.concat(
        quickSort( xs.filter (_ <= x) )
        , x::Nil
        , quickSort( xs.filter (_ > x) )
      )
  }
}

val arr = (1 to 25).toList.reverse
println(s"input     --> $arr")
println(s"quickSort --> ${quickSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input     --> $arr2")
println(s"quickSort --> ${quickSort(arr2)}")
