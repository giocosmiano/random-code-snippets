/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n log n

-- Haskell
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
*/

def mergeSort[T](list: List[T])(implicit ev$1: T => Ordered[T]): List[T] = {

  def merge(tuple: (List[T], List[T])): List[T] = {
    tuple match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x::xs, y::ys) =>
        if (x <= y) x :: merge( (xs, y::ys) )
        else y :: merge( (x::xs, ys) )
    }
  }

  list match {
    case Nil => Nil
    case x::Nil => x::Nil
    case xs =>
      val half = xs.length / 2
      val firstHalf  = mergeSort( xs.take(half) )
      val secondHalf = mergeSort( xs.drop(half) )
      merge( (firstHalf, secondHalf) )
  }
}

val arr = (1 to 25).toList.reverse
println(s"input     --> $arr")
println(s"mergeSort --> ${mergeSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input     --> $arr2")
println(s"mergeSort --> ${mergeSort(arr2)}")
