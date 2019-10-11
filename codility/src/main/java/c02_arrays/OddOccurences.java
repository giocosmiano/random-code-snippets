package c02_arrays;

/*
A non-empty array A consisting of N integers is given. The array contains an odd number of elements,
and each element of the array can be paired with another element that has the same value, except for one element that is left unpaired.

For example, in array A such that:
  A[0] = 9  A[1] = 3  A[2] = 9
  A[3] = 3  A[4] = 9  A[5] = 7
  A[6] = 9

        the elements at indexes 0 and 2 have value 9,
        the elements at indexes 1 and 3 have value 3,
        the elements at indexes 4 and 6 have value 9,
        the element at index 5 has value 7 and is unpaired.

Write a function:

    class Solution { public int solution(int[] A); }

that, given an array A consisting of N integers fulfilling the above conditions, returns the value of the unpaired element.

For example, given array A such that:
  A[0] = 9  A[1] = 3  A[2] = 9
  A[3] = 3  A[4] = 9  A[5] = 7
  A[6] = 9

the function should return 7, as explained in the example above.

Write an efficient algorithm for the following assumptions:

        N is an odd integer within the range [1..1,000,000];
        each element of array A is an integer within the range [1..1,000,000,000];
        all but one of the values in A occur an even number of times.

*/

import java.util.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class OddOccurences {

  private static List<Integer> solution(final List<Integer> list) {

/*
    List<Integer> nonPaired = new ArrayList<>();
    Map<Integer, List<Integer>> mapOfInts =
            list.stream()
                    .collect(Collectors.groupingBy(e -> e));

    mapOfInts.forEach((k, v) -> {
      if (v.size() % 2 > 0) {
        nonPaired.add(k);
      };
    });

    return nonPaired;
*/

    return
            list.stream()
                    .collect(Collectors.groupingBy(e -> e))
                    .entrySet()
                    .stream()
                    .filter(e -> e.getValue().size() % 2 > 0)
                    .map(Map.Entry::getKey)
                    .collect(Collectors.toList());
  }

  public static void main(String[] args) {

    List<List<Integer>> listOfList = Arrays.asList(
            Arrays.asList(9, 3, 9, 3, 9, 7, 9)
           , Arrays.asList(9, 3, 9, 3, 9, 7)
    );

    for (List<Integer> list : listOfList) {
      List<Integer> nonPaired = solution(list);
      System.out.println(String.format("List %s, Non-Paired List %s", list, nonPaired));
    }
  }
}