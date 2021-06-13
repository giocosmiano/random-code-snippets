/*
https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
time complexity --> n²

-- Haskell
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  |  x < y = x:y:ys
  | otherwise = y : insert x ys

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
*/

def insertSort[T](list: List[T])(implicit ev$1: T => Ordered[T]): List[T] = {

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
    case x::xs => insert( (x, insertSort(xs)) )
  }
}

val arr = (1 to 25).toList.reverse
println(s"input      --> $arr")
println(s"insertSort --> ${insertSort(arr)}")

val arr2 = ('a' to 'z').toList.reverse
println(s"input      --> $arr2")
println(s"insertSort --> ${insertSort(arr2)}")
