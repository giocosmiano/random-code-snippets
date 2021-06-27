package sorting;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static sorting.CollectionUtils.*;

public class SelectionSort {

  /**
  https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
  time complexity --> n²

  -- Haskell
  deleteFromOri :: Eq a => a -> [a] -> [a]
  deleteFromOri _ [] = []
  deleteFromOri x (y:ys)
    | x == y = ys
    | otherwise = y:deleteFromOri x ys

  selectionSort :: Ord a => [a] -> [a]
  selectionSort [] = []
  selectionSort xs = mini : selectionSort xs'
    where
      mini = minimum xs
      xs' = deleteFromOri mini xs
  */
  private static <T extends Comparable<T>> List<T> deleteFromOri(final T elem, final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty()) {
      return allObjects;
    }

    ImmutablePair<T, List<T>> deStructList = getHeadAndTail( allObjects );
    T head = deStructList.getLeft();
    List<T> tail = deStructList.getRight();

    List<T> newList  = new ArrayList<>();
    if (elem.compareTo(head) == 0) {
      newList.addAll( tail );

    } else {
      newList.add( head );
      newList.addAll( deleteFromOri( elem, tail ) );
    }

    return newList;
  }

  public static <T extends Comparable<T>> List<T> selectionSort(final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty()) {
      return allObjects;
    }

    T mini = streamOfObjects(allObjects).min(T::compareTo).orElse(null);
    List<T> newList2 = deleteFromOri( mini, allObjects );

    List<T> newList = new ArrayList<>();
    newList.add( mini );
    newList.addAll( selectionSort( newList2 ) );

    return newList;
  }

  public static void main(String[] args) {
    List<Integer> ints =
            IntStream.rangeClosed(1, 25)
                     .boxed()
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input         --> " + ints);
    System.out.println("selectionSort --> " + selectionSort( ints ));

    List<Character> chars =
            IntStream.rangeClosed('a', 'z')
                     .mapToObj(e -> (char)e)
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input         --> " + chars);
    System.out.println("selectionSort --> " + selectionSort( chars ));

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
    boolean isBigArraySorted = selectionSort(unSorted).equals(sorted);
    watch.stop();
    System.out.println("Time Elapsed: " + watch.getTime() / 1000.0 + " secs");
    System.out.printf("isBigArraySorted using `selectionSort` with %s elements --> %s", unSorted.size(), isBigArraySorted);
  }
}