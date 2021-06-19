/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n²

-- Haskell
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  |  x < y = x:y:ys
  | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
*/

// https://docs.scala-lang.org/cheatsheets/index.html
// https://docs.scala-lang.org/tutorials/FAQ/index.html
// https://www.baeldung.com/scala/view-context-bounds
// https://twitter.github.io/scala_school/advanced-types.html
// https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
// def insertionSort[T <% Ordered[T]](list: => List[T]): List[T] = {

// using Scala v2.12, Stream
import scala.collection.immutable.Stream.#::

def insertionSort[T](list: => Stream[T])(implicit evidence: T => Ordered[T]): Stream[T] = {

  def insert(tuple: => (T, Stream[T])): Stream[T] = {
    tuple match {
      case (x, Stream.Empty)    => x #:: Stream.empty
      case (x, allYs@ y #:: ys) =>
        if (x < y) x #:: allYs
        else       y #:: insert( (x, ys) )
    }
  }

  list match {
    case Stream.Empty => Stream.empty
    case x #:: xs     => insert( (x, insertionSort(xs)) )
  }
}

lazy val arr = (25 to 1 by -1).to(Stream)
println(s"input         --> ${arr.toList}")
println(s"insertionSort --> ${insertionSort(arr).toList}")

lazy val arr2 = ('a' to 'z').to(Stream).reverse
println(s"input         --> ${arr2.toList}")
println(s"insertionSort --> ${insertionSort(arr2).toList}")