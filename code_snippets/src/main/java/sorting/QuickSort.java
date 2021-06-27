package sorting;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static sorting.CollectionUtils.*;

public class QuickSort {

  /**
  https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_of_algorithms
  time complexity --> n log n

  -- Haskell
  quickSort :: Ord a => [a] -> [a]
  quickSort [] = []
  quickSort (x:xs) =
      let smallerOrEqual = filter (<= x) xs
          larger = filter (> x) xs
      in  quickSort smallerOrEqual ++ [x] ++ quickSort larger
  */
  public static <T extends Comparable<T>> List<T> quickSort(final List<T> list) {
    List<T> allObjects = listOfObjects(list);
    if (allObjects.isEmpty()) {
      return allObjects;
    }

    ImmutablePair<T, List<T>> deStructList = getHeadAndTail( allObjects );
    T head = deStructList.getLeft();
    List<T> tail = deStructList.getRight();

    List<T> smallerOrEqual = streamOfObjects( tail ).filter(e -> e.compareTo( head ) <= 0).collect( Collectors.toList() );
    List<T> larger         = streamOfObjects( tail ).filter(e -> e.compareTo( head ) >  0).collect( Collectors.toList() );

    List<T> newList = new ArrayList<>();
    newList.addAll( quickSort( smallerOrEqual ) );
    newList.addAll( Collections.singletonList( head ) );
    newList.addAll( quickSort( larger ) );

    return newList;
  }

  public static void main(String[] args) {
    List<Integer> ints =
            IntStream.rangeClosed(1, 25)
                     .boxed()
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input     --> " + ints);
    System.out.println("quickSort --> " + quickSort( ints ));

    List<Character> chars =
            IntStream.rangeClosed('a', 'z')
                     .mapToObj(e -> (char)e)
                     .sorted(Collections.reverseOrder())
                     .collect(Collectors.toList());
    System.out.println("input     --> " + chars);
    System.out.println("quickSort --> " + quickSort( chars ));

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
    boolean isBigArraySorted = quickSort(unSorted).equals(sorted);
    watch.stop();
    System.out.println("Time Elapsed: " + watch.getTime() / 1000.0 + " secs");
    System.out.printf("isBigArraySorted using `quickSort` with %s elements --> %s", unSorted.size(), isBigArraySorted);
  }
}