package c03_timeComplexity

/*
  An array A consisting of N different integers is given. The array contains integers
  in the range [1..(N + 1)], which means that exactly one element is missing.

  Your goal is to find that missing element.

  Write a function:

  class Solution { public int solution(int[] A); }

  that, given an array A, returns the value of the missing element.

  For example, given array A such that:
  A[0] = 2
  A[1] = 3
  A[2] = 1
  A[3] = 5

  the function should return 4, as it is the missing element.

  Write an efficient algorithm for the following assumptions:

  N is an integer within the range [0..100,000];
  the elements of A are all distinct;
  each element of array A is an integer within the range [1..(N + 1)].
*/

object PermMissingElemScala extends App {

  def solution(list: List[Int]): Int = {

    val sorted = list.sorted
    val missing =
      if (sorted.isEmpty) 1
      else if (sorted.size == 1) 2
      else {
        import scala.annotation.tailrec
        @tailrec
        def findMissingElem(nbr: Int, subList: List[Int]): Int = {
          subList match {
            case head :: tail =>
              if (head != nbr + 1) nbr + 1
              else findMissingElem(head, tail)
            case Nil => nbr
          }
        }
        findMissingElem(0, sorted)
      }

    missing
  }

  def solutionWithLoop(list: List[Int]): Int = {
    val sorted = list.sorted.view.zipWithIndex
    val missing =
      if (sorted.isEmpty) 1
      else if (sorted.size == 1) 2
      else {
        var missingNbr = 0
        import scala.util.control.Breaks._
        breakable {
          for((nbr,idx) <- sorted) {
            if (nbr != idx+1) {
              missingNbr = idx+1
              break
            }
          }
        }
        missingNbr
      }

    missing
  }

  val list = List(2, 3, 1, 5)
  println(s"$list, Missing ${solution(list)}, Missing using Loop ${solutionWithLoop(list)}")
}
