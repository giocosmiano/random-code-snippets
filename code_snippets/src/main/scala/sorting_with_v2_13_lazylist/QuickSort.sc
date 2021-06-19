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

// https://docs.scala-lang.org/cheatsheets/index.html
// https://docs.scala-lang.org/tutorials/FAQ/index.html
// https://www.baeldung.com/scala/view-context-bounds
// https://twitter.github.io/scala_school/advanced-types.html
// https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
// def quickSort[T <% Ordered[T]](list: => List[T]): List[T] = {

// using Scala v2.13, LazyList
import scala.collection.immutable.LazyList.#::

def quickSort[T](list: => LazyList[T])(implicit evidence: T => Ordered[T]): LazyList[T] = {

  list match {
    case LazyList() => LazyList.empty
    case x #:: xs   => quickSort( xs.filter (_ <= x) ) #::: x #:: LazyList.empty #::: quickSort( xs.filter (_ > x) )
  }
}

lazy val arr = (25 to 1 by -1).to(LazyList)
println(s"input     --> ${arr.toList}")
println(s"quickSort --> ${quickSort(arr).toList}")

lazy val arr2 = ('a' to 'z').to(LazyList).reverse
println(s"input     --> ${arr2.toList}")
println(s"quickSort --> ${quickSort(arr2).toList}")
