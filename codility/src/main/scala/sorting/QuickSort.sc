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
// def quickSort[T <% Ordered[T]](list: List[T]): List[T] = {
def quickSort[T](list: List[T])(implicit evidence: T => Ordered[T]): List[T] = {

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

val arr = (25 to 1 by -1).toList
println(s"input     --> $arr")
println(s"quickSort --> ${quickSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input     --> $arr2")
println(s"quickSort --> ${quickSort(arr2)}")
