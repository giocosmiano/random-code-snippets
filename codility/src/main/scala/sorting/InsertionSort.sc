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
// def insertionSort[T <% Ordered[T]](list: List[T]): List[T] = {
def insertionSort[T](list: List[T])(implicit evidence: T => Ordered[T]): List[T] = {

  def insert(tuple: (T, List[T])): List[T] = {
    tuple match {
      case (x, Nil) => x::Nil
      case (x, y::ys) =>
        if (x < y) x :: y :: ys
        else y :: insert( (x, ys) )
    }
  }

  list match {
    case Nil => Nil
    case x::xs => insert( (x, insertionSort(xs)) )
  }
}

val arr = (25 to 1 by -1).toList
println(s"input         --> $arr")
println(s"insertionSort --> ${insertionSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input         --> $arr2")
println(s"insertionSort --> ${insertionSort(arr2)}")
