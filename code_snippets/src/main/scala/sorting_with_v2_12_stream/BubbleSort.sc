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

// https://docs.scala-lang.org/cheatsheets/index.html
// https://docs.scala-lang.org/tutorials/FAQ/index.html
// https://www.baeldung.com/scala/view-context-bounds
// https://twitter.github.io/scala_school/advanced-types.html
// https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
// def bubbleSort[T <% Ordered[T]](list: => List[T]): List[T] = {

// using Scala v2.12, Stream
import scala.collection.immutable.Stream.#::
import scala.annotation.tailrec

@tailrec
def bubbleSort[T](list: => Stream[T])(implicit evidence: T => Ordered[T]): Stream[T] = {

  def swaps(xs: => Stream[T]): Stream[T] = {
    xs match {
      case Stream.Empty        => Stream.empty
      case x  #:: Stream.Empty => x #:: Stream.empty
      case x1 #:: x2 #:: xs  =>
        if (x1 >= x2) x2 #:: swaps( x1 #:: xs)
        else          x1 #:: swaps( x2 #:: xs)
    }
  }

  list match {
    case Stream.Empty => Stream.empty
    case xs =>
      if  (swaps(xs) == xs) xs
      else bubbleSort( swaps(xs) )
  }
}

lazy val arr = (25 to 1 by -1).to(Stream)
println(s"input      --> ${arr.toList}")
println(s"bubbleSort --> ${bubbleSort(arr).toList}")

lazy val arr2 = ('a' to 'z').to(Stream).reverse
println(s"input      --> ${arr2.toList}")
println(s"bubbleSort --> ${bubbleSort(arr2).toList}")
