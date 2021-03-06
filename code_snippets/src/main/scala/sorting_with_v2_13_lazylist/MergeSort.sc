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

// https://docs.scala-lang.org/cheatsheets/index.html
// https://docs.scala-lang.org/tutorials/FAQ/index.html
// https://www.baeldung.com/scala/view-context-bounds
// https://twitter.github.io/scala_school/advanced-types.html
// https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
// def mergeSort[T <% Ordered[T]](list: => List[T]): List[T] = {

// using Scala v2.13, LazyList
import scala.collection.immutable.LazyList.#::

def mergeSort[T](list: => LazyList[T])(implicit evidence: T => Ordered[T]): LazyList[T] = {

  def merge(tuple: => (LazyList[T], LazyList[T])): LazyList[T] = {

    tuple match {
      case (xs, LazyList()) => xs
      case (LazyList(), ys) => ys
      case (allXs@ x #:: xs, allYs@ y #:: ys) =>
        if (x <= y) x #:: merge( xs, allYs )
        else        y #:: merge( allXs, ys )
    }
  }

  list match {
    case LazyList()       => LazyList.empty
    case x #:: LazyList() => x #:: LazyList.empty
    case xs =>
      lazy val half = xs.length / 2
      lazy val firstHalf  = mergeSort( xs.take(half) )
      lazy val secondHalf = mergeSort( xs.drop(half) )
      merge( (firstHalf, secondHalf) )
  }
}

// https://biercoff.com/easily-measuring-code-execution-time-in-scala/
def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  val elapsed = t1 - t0
  val seconds = elapsed / 1_000_000_000.0
  println("Elapsed time: " + seconds + " secs")
  result
}

lazy val arr = (25 to 1 by -1).to(LazyList)
println(s"input     --> ${arr.toList}")
println(s"mergeSort --> ${mergeSort(arr).toList}")

lazy val arr2 = ('a' to 'z').to(LazyList).reverse
println(s"input     --> ${arr2.toList}")
println(s"mergeSort --> ${mergeSort(arr2).toList}")

// https://biercoff.com/easily-measuring-code-execution-time-in-scala/
// limiting to 4k as JDK will throw java.lang.StackOverflowError
lazy val unSortedBigArray = (2500000 to 1 by -1).to(LazyList)
lazy val sortedBigArray   = (1 to 2500000).to(LazyList)
lazy val isBigArraySorted = time { mergeSort(unSortedBigArray).toList == sortedBigArray.toList }
println(s"isBigArraySorted using `mergeSort` with ${unSortedBigArray.length} elements --> $isBigArraySorted")


