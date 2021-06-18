package sorting;

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
  }
}