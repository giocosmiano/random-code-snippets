package sorting;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static sorting.CollectionUtils.*;

public class BubbleSort {

  /**
  https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
  time complexity --> n²

  -- Haskell
  swaps :: Ord a => [a] -> [a]
  swaps [] = []
  swaps [x] = [x]
  swaps (x1:x2:xs)
    | x1 > x2 = x2 : swaps(x1:xs)
    | otherwise = x1 : swaps(x2:xs)

  bubbleSort :: Ord a => [a] -> [a]
  bubbleSort [] = []
  bubbleSort xs
    | swaps xs == xs = xs -- did not change, stop
    | otherwise = bubbleSort $ swaps xs
  */
  private static <T extends Comparable<T>> List<T> swaps(final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty() || allObjects.size() == 1) {
      return allObjects;
    }

    ImmutablePair<T, List<T>> deStructList = getHeadAndTail( allObjects );
    T head = deStructList.getLeft();

    ImmutablePair<T, List<T>> deStructList2 = getHeadAndTail( deStructList.getRight() );
    T head2 = deStructList2.getLeft();
    List<T> tail2 = deStructList2.getRight();

    List<T> newList  = new ArrayList<>();
    List<T> newList2 = new ArrayList<>();
    if (head.compareTo(head2) > 0) {
      newList2.add( head );
      newList2.addAll( tail2 );

      newList.add( head2 );

    } else {
      newList2.add( head2 );
      newList2.addAll( tail2 );

      newList.add( head );
    }
    newList.addAll( swaps( newList2 ) );

    return newList;
  }

  public static <T extends Comparable<T>> List<T> bubbleSort(final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty()) {
      return allObjects;
    }

    if (swaps(allObjects).equals(allObjects)) {
      return allObjects;
    }

    return bubbleSort( swaps( allObjects ) );
  }

  public static void main(String[] args) {
    List<Integer> ints =
            IntStream.rangeClosed(1, 25)
                     .boxed()
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input      --> " + ints);
    System.out.println("bubbleSort --> " + bubbleSort( ints ));

    List<Character> chars =
            IntStream.rangeClosed('a', 'z')
                     .mapToObj(e -> (char)e)
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input      --> " + chars);
    System.out.println("bubbleSort --> " + bubbleSort( chars ));
  }
}