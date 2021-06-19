/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n²

-- Haskell
deleteFromOri :: Eq a => a -> [a] -> [a]
deleteFromOri _ [] = []
deleteFromOri x (y:ys)
  | x == y = ys
  | otherwise = y:deleteFromOri x ys

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
  where
    mini = minimum xs
    xs' = deleteFromOri mini xs
*/

// https://docs.scala-lang.org/cheatsheets/index.html
// https://docs.scala-lang.org/tutorials/FAQ/index.html
// https://www.baeldung.com/scala/view-context-bounds
// https://twitter.github.io/scala_school/advanced-types.html
// https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
// def selectionSort[T <% Ordered[T]](list: => List[T]): List[T] = {

// using Scala v2.13, LazyList
import scala.collection.immutable.LazyList.#::

def selectionSort[T](list: => LazyList[T])(implicit evidence: T => Ordered[T]): LazyList[T] = {

  def deleteFromOri(tuple: => (T, LazyList[T])): LazyList[T] = {
    tuple match {
      case (_, LazyList()) => LazyList.empty
      case (x, y #:: ys )  =>
        if (x == y) ys
        else        LazyList.cons(y, deleteFromOri( (x, ys) ))
    }
  }

  list match {
    case LazyList() => LazyList.empty
    case xs =>
      lazy val min = xs.min
      lazy val ys = deleteFromOri( (min, xs) )
      LazyList.cons(min, selectionSort(ys))
  }
}

lazy val arr = (25 to 1 by -1).to(LazyList)
println(s"input         --> ${arr.toList}")
println(s"selectionSort --> ${selectionSort(arr).toList}")

lazy val arr2 = ('a' to 'z').to(LazyList).reverse
println(s"input         --> ${arr2.toList}")
println(s"selectionSort --> ${selectionSort(arr2).toList}")
