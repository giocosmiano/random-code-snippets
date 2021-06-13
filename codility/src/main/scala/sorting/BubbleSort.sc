/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n²

-- Haskell
swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
  | x1 > x2 = x2 : swaps(x1:xs)
  | otherwise = x1 : swaps(x2:xs)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs
  | swaps xs == xs = xs -- did not change, stop
  | otherwise = bubbleSort $ swaps xs
*/

import scala.annotation.tailrec

@tailrec
def bubbleSort[T](list: List[T])(implicit ev$1: T => Ordered[T]): List[T] = {

  def swaps(xs: List[T]): List[T] = {
    xs match {
      case Nil => Nil
      case x::Nil => x::Nil
      case x1::x2::xs =>
        if (x1 >= x2) x2 :: swaps( x1::xs )
        else x1 :: swaps( x2::xs )
    }
  }

  list match {
    case Nil => Nil
    case xs =>
      if (swaps(xs) == xs) xs
      else bubbleSort( swaps(xs) )
  }
}

val arr = (1 to 25).toList.reverse
println(s"input      --> $arr")
println(s"bubbleSort --> ${bubbleSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input      --> $arr2")
println(s"bubbleSort --> ${bubbleSort(arr2)}")
