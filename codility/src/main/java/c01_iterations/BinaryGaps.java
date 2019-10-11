package c01_iterations;

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

import java.util.*;
import java.util.stream.Collectors;

public class BinaryGaps {

  private static String toBinaryString(int n) {
    return Integer.toBinaryString(n);
  }

  private static int solution(int n) {
    int max = 0;
    String s = toBinaryString(n);
    List<String> lStr = new ArrayList<>(Arrays.asList(s.split("1")));

    // e.g. Int 15, binary 1111 and there's no gaps so essentially the list is empty thus 0
    if (lStr.size() > 0) {

      // e.g. Int 20, binary 10100 and therefore exclude the `last` element to get the max length of binary 0's
      String lastChar = s.substring(s.length() - 1);
      if (lastChar.equals("0")) {
        lStr.remove(lStr.size() - 1);
      }

      // get the max length of binary 0's
      max = lStr.stream()
              .mapToInt(String::length)
              .max().orElse(0);
    }

//    System.out.println(String.format("Binary %s, List %s, Binary gap %s", s, lStr, max));
    return max;
  }

  private static int solutionInEfficient(int n) {
    String s = toBinaryString(n);
    List<Character> lStr = s.chars().mapToObj(e -> (char)e).collect(Collectors.toList());

    boolean ok = false;
    int max = 0;
    int ctr = 0;
    for (Character c : lStr) {
      if (ok) {
        if (c == '0') {
          ctr++;
        } else {
          if (ctr > max) {
            max = ctr;
          }
          ctr = 0;
        }
      }
      if (c == '1') {
        ok = true;
      }
    }

//    System.out.println(String.format("Binary %s, List %s, Binary gap %s", s, lStr, max));
    return max;
  }

  public static void main(String[] args) {
    List<Integer> list = Arrays.asList(15, 20, 32, 529, 1041, 1041000000, 2147483647);
    for (Integer n : list) {
      String s = toBinaryString(n);
      int s1 = solution(n);
      int s2 = solutionInEfficient(n);
      System.out.println(String.format("Int %s, Binary %s, Gaps %s, Gaps using In-Efficient %s", n, s, s1, s2));
    }
  }
}