package utilities;

import java.util.*;
import java.util.stream.*;

public class ZipListToList {

    public static <T> List<T> zipListToList(final List<List<T>> listOfList) {
        final Integer maxSize =
                Optional.ofNullable(listOfList)
                        .orElse(Collections.emptyList())
                        .stream()
                        .filter(Objects::nonNull) // ignore un-initialized list
                        .map(List::size)
                        .max(Integer::compare)
                        .orElse(0);

        return IntStream.range(0, maxSize)
                        .mapToObj(i ->
                                listOfList.stream()
                                          .filter(Objects::nonNull) // ignore un-initialized list
                                          .map(list -> {
                                              T elem = null;
                                              if (i < list.size()) {
                                                  elem = list.get(i);
                                              }
                                              return elem;
                                          })
                                          .filter(Objects::nonNull) // ignore un-initialized element
                                          .collect(Collectors.toList())
                        )
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList());
    }

    public static void main(String[] args) {
        runSampleZipListToList();
    }

    public static void runSampleZipListToList() {
        List<Integer> ints0 = null;
        List<Integer> ints1 = Arrays.asList(11, 12, 13, 14, null, 15);
        List<Integer> ints2 = Arrays.asList(21, 22, null, 23, 24);
        List<Integer> ints3 = Arrays.asList(31, null, 32, 33);
        List<Integer> empty = Collections.emptyList();

        List<List<Integer>> listOfList = Collections.singletonList(empty);
        List<Integer> results = zipListToList(listOfList);
        printSampleZipListToList(listOfList, results);

        listOfList = Arrays.asList(ints1, ints0, ints2, ints3);
        results = zipListToList(listOfList);
        printSampleZipListToList(listOfList, results);

        listOfList = Arrays.asList(ints3, ints0, ints2, ints1);
        results = zipListToList(listOfList);
        printSampleZipListToList(listOfList, results);
    }

    public static <T> void printSampleZipListToList(final List<List<T>> listOfList, final List<T> results) {
        System.out.println();
        System.out.println("List :          " + listOfList);
        System.out.println("ZipListToList : " + results);
    }
}
