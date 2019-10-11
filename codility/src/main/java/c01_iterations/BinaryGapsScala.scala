package c01_iterations

/*
  A binary gap within a positive integer N is any maximal sequence of consecutive zeros
  that is surrounded by ones at both ends in the binary representation of N.

  For example, number 9 has binary representation 1001 and contains a binary gap of length 2.
  The number 529 has binary representation 1000010001 and contains two binary gaps:
  one of length 4 and one of length 3. The number 20 has binary representation 10100 and
  contains one binary gap of length 1. The number 15 has binary representation 1111 and
  has no binary gaps. The number 32 has binary representation 100000 and has no binary gaps.

  Write a function:

  class Solution { public int solution(int N); }

  that, given a positive integer N, returns the length of its longest binary gap.
  The function should return 0 if N doesn't contain a binary gap.

  For example, given N = 1041 the function should return 5, because N has
  binary representation 10000010001 and so its longest binary gap is of length 5.
  Given N = 32 the function should return 0, because N has binary representation '100000' and thus no binary gaps.

  Write an efficient algorithm for the following assumptions:

  N is an integer within the range [1..2,147,483,647].
*/

object BinaryGapsScala extends App {

  def toBinaryString(n: Int): String = {
    Integer.toBinaryString(n)
  }

  def solution(n: Int): Int = {
    val s = toBinaryString(n)
    val lStr = s.split("1").toList
    val max =
    // e.g. Int 15, binary 1111 and there's no gaps so essentially the list is empty thus 0
      if (lStr.isEmpty) 0
      // e.g. Int 20, binary 10100 and therefore exclude the `last` element to get the max length of binary 0's
      else if (s.toList.last == '0') lStr.init.map(e => e.length).max
      // get the max length of binary 0's
      else lStr.map(e => e.length).max
    //    println(s"Input $n, Binary $bStr, List $lStr, Binary gap $max")
    max
  }

  def solutionInEfficient(n: Int): Int = {
    val s = toBinaryString(n)
    val lStr = s.toList
    var ok = false
    var max = 0
    var ctr = 0
    for (c <- lStr) {
      if (ok) if (c == '0') ctr += 1
      else {
        if (ctr > max) max = ctr
        ctr = 0
      }
      if (c == '1') ok = true
    }
    //    println(s"Input $n, Binary $bStr, Binary length ${lStr.size}, Binary gap $max")
    max
  }

  val list = List(15, 20, 32, 529, 1041, 1041000000, 2147483647)
  list.foreach(n => {
    val s = toBinaryString(n)
    val s1 = solution(n)
    val s2 = solutionInEfficient(n)
    println(s"Int $n, Binary $s, Gaps $s1, Gaps using In-Efficient $s2")
  })
}
