package c03_timeComplexity;

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

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class PermMissingElem {

  private static int solution(final List<Integer> list) {
    int missing = 1;
    if (list != null && list.size() > 0) {
      int length = list.size();
      List<Integer> sorted = list.stream().sorted().collect(Collectors.toList());

      if (length == 1) {
        missing += 1;

      } else {
        missing = sorted.get(length - 1) + 1;
        for (int i=0; i<length; i++) {
          int a = sorted.get(i);
          if (i < length - 1) {
            int b = sorted.get(i + 1) - 1;
            if (a != b) {
              missing = b;
              break;
            }
          }
        }
      }
    }
    return missing;
  }

  public static void main(String[] args) {

    List<Integer> list = Arrays.asList(2, 3, 1, 5);
    System.out.println(String.format("List %s, Missing %s", list, solution(list)));
  }
}