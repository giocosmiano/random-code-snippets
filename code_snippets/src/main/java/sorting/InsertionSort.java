package sorting;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static sorting.CollectionUtils.*;

public class InsertionSort {

  /**
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
  private static <T extends Comparable<T>> List<T> insert(final T elem, final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty()) {
      return Collections.singletonList(elem);
    }

    ImmutablePair<T, List<T>> deStructList = getHeadAndTail( allObjects );
    T head = deStructList.getLeft();
    List<T> tail = deStructList.getRight();

    List<T> newList  = new ArrayList<>();
    if (elem.compareTo(head) < 0) {
      newList.add( elem );
      newList.add( head );
      newList.addAll( tail );

    } else {
      newList.add( head );
      newList.addAll( insert( elem, tail ) );
    }

    return newList;
  }

  public static <T extends Comparable<T>> List<T> insertionSort(final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty()) {
      return allObjects;
    }

    ImmutablePair<T, List<T>> deStructList = getHeadAndTail( allObjects );
    T head = deStructList.getLeft();
    List<T> tail = deStructList.getRight();

    return insert( head, insertionSort( tail ));
  }

  public static void main(String[] args) {
    List<Integer> ints =
            IntStream.rangeClosed(1, 25)
                     .boxed()
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input         --> " + ints);
    System.out.println("insertionSort --> " + insertionSort( ints ));

    List<Character> chars =
            IntStream.rangeClosed('a', 'z')
                     .mapToObj(e -> (char)e)
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input         --> " + chars);
    System.out.println("insertionSort --> " + insertionSort( chars ));

    List<Integer> sorted =
            IntStream.rangeClosed(1, 1000)
                     .boxed()
                     .collect(Collectors.toList());
    List<Integer> unSorted =
            sorted.stream()
                  .sorted(Collections.reverseOrder())
                  .collect(Collectors.toList());

    StopWatch watch = new StopWatch();
    watch.start();
    boolean isBigArraySorted = insertionSort(unSorted).equals(sorted);
    watch.stop();
    System.out.println("Time Elapsed: " + watch.getTime() / 1000.0 + " secs");
    System.out.printf("isBigArraySorted using `insertionSort` with %s elements --> %s", unSorted.size(), isBigArraySorted);
  }
}