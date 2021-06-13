package sorting;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static sorting.CollectionUtils.*;

public class MergeSort {

  /**
  https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
  time complexity --> n log n

  -- Haskell
  mergeSort :: Ord a => [a] -> [a]
  mergeSort [] = []
  mergeSort [x] = [x]
  mergeSort xs = merge firstHalf secondHalf
    where
      half = length xs `div` 2
      firstHalf  = mergeSort $ take half xs
      secondHalf = mergeSort $ drop half xs

      merge :: (Ord a) => [a] -> [a] -> [a]
      merge x [] = x
      merge [] y = y
      merge allX@(x:xs) allY@(y:ys)
        | x <= y    = x : merge xs allY
        | otherwise = y : merge allX ys
  */
  private static <T extends Comparable<T>> List<T> merge(final List<T> first, final List<T> second) {

    // remove `null` elements from firstHalf and secondHalf lists
    List<T> firstHalf  = listOfObjects( first );
    List<T> secondHalf = listOfObjects( second );
    if (secondHalf.isEmpty()) {
      return firstHalf;

    } else if (firstHalf.isEmpty()) {
      return secondHalf;
    }

    ImmutablePair<T, List<T>> deStructFirst  = getHeadAndTail( firstHalf  );
    ImmutablePair<T, List<T>> deStructSecond = getHeadAndTail( secondHalf );
    T firstHead  = deStructFirst.getLeft();
    T secondHead = deStructSecond.getLeft();
    List<T> firstTail  = deStructFirst.getRight();
    List<T> secondTail = deStructSecond.getRight();

    List<T> newList = new ArrayList<>();
    if (firstHead.compareTo(secondHead) <= 0) {
      newList.add( firstHead );
      newList.addAll( merge( firstTail, secondHalf ) );

    } else {
      newList.add( secondHead );
      newList.addAll( merge( firstHalf, secondTail ) );
    }

    return newList;
  }

  public static <T extends Comparable<T>> List<T> mergeSort(final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty() || allObjects.size() == 1) {
      return allObjects;
    }

    int half = allObjects.size() / 2;
    List<T> firstHalf  = mergeSort( streamOfObjects( allObjects ).limit( half ).collect( Collectors.toList() ) );
    List<T> secondHalf = mergeSort( streamOfObjects( allObjects ).skip(  half ).collect( Collectors.toList() ) );

    return merge( firstHalf, secondHalf );
  }

  public static void main(String[] args) {
    List<Integer> ints =
            IntStream.rangeClosed(1, 25)
                     .boxed()
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input     --> " + ints);
    System.out.println("mergeSort --> " + mergeSort( ints ));

    List<Character> chars =
            IntStream.rangeClosed('a', 'z')
                     .mapToObj(e -> (char)e)
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input     --> " + chars);
    System.out.println("mergeSort --> " + mergeSort( chars ));
  }
}