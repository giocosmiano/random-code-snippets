package utilities;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class ZipWithList {

    /**
     * Implementation of haskell `zipWith` function,  while optionally allowing `null` value elements
     * http://zvon.org/other/haskell/Outputprelude/zipWith_f.html
     * https://hoogle.haskell.org/?hoogle=zipwith
     */
    public static <L, R, T> List<T> zipWithList(final BiFunction<L, R, T> zipper, final List<L> leftList, final List<R> rightList) {
        return zipWithList(zipper, leftList, rightList, false);
    }

    public static <L, R, T> List<T> zipWithList(final BiFunction<L, R, T> zipper, final List<L> leftList, final List<R> rightList, final Boolean isToAllowNull) {
        final Predicate<T> checkForNull = e -> isToAllowNull || Objects.nonNull(e);

        final Integer minSize =
                Optional.ofNullable(leftList)
                        .map(e ->
                                Math.min(e.size(), Optional.ofNullable(rightList)
                                                           .orElse(Collections.emptyList())
                                                           .size())
                        )
                        .orElse(0);

        return IntStream.range(0, minSize)
                        .mapToObj(i -> {
                            L left = Optional.ofNullable(leftList)
                                             .flatMap(e -> Optional.ofNullable(e.get(i)))
                                             .orElse(null);
                            R right = Optional.ofNullable(rightList)
                                              .flatMap(e -> Optional.ofNullable(e.get(i)))
                                              .orElse(null);
                            return Optional.ofNullable(left)
                                           .flatMap(l -> Optional.ofNullable(right).map(r -> zipper.apply(l, r)))
                                           .orElse(null);
                        })
                        .filter(checkForNull)
                        .collect(Collectors.toList());
    }

    public static void main(String[] args) {
        runSampleZipWithList();
    }

    public static void runSampleZipWithList() {
        List<Integer> ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        List<Integer> ints2 = Arrays.asList(11, 12, 13, null, 14, 15);
        List<String> chars = Arrays.asList("a", "b", "c", "d", null);
        List<Boolean> bools = Arrays.asList(true, false, null, true);
        List<Integer> empty = Collections.emptyList();

        BiFunction<Integer, Integer, Integer> sum = Integer::sum;
        BiFunction<Integer, Integer, Integer> product = (a, b) -> a * b;
        BiFunction<Integer, Integer, Integer> multiplyBy2ThenAdd = (a, b) -> 2 * a + b;
        BiFunction<Integer, String, ImmutablePair<Integer, String>> pair1 = ImmutablePair::new;
        BiFunction<Boolean, String, ImmutablePair<Boolean, String>> pair2 = ImmutablePair::new;
        BiFunction<Integer, Boolean, ImmutablePair<Integer, Boolean>> pair3 = ImmutablePair::new;

        List<Integer> results1;
        results1 = zipWithList(sum, empty, ints1);
        printSampleZipWithList("sum", empty, ints1, results1, false);

        results1 = zipWithList(sum, empty, ints1, true);
        printSampleZipWithList("sum", empty, ints1, results1, true);

        results1 = zipWithList(sum, ints1, ints2);
        printSampleZipWithList("sum", ints1, ints2, results1, false);

        results1 = zipWithList(sum, ints1, ints2, true);
        printSampleZipWithList("sum", ints1, ints2, results1, true);

        results1 = zipWithList(product, ints1, ints2);
        printSampleZipWithList("product", ints1, ints2, results1, false);

        results1 = zipWithList(product, ints1, ints2, true);
        printSampleZipWithList("product", ints1, ints2, results1, true);

        results1 = zipWithList(multiplyBy2ThenAdd, ints1, ints2);
        printSampleZipWithList("multiplyBy2ThenAdd", ints1, ints2, results1, false);

        results1 = zipWithList(multiplyBy2ThenAdd, ints1, ints2, true);
        printSampleZipWithList("multiplyBy2ThenAdd", ints1, ints2, results1, true);

        List<ImmutablePair<Integer, String>> results2;
        results2 = zipWithList(pair1, ints1, chars);
        printSampleZipWithList("pair1", ints1, chars, results2, false);

        results2 = zipWithList(pair1, ints1, chars, true);
        printSampleZipWithList("pair1", ints1, chars, results2, true);

        List<ImmutablePair<Boolean, String>> results3;
        results3 = zipWithList(pair2, bools, chars);
        printSampleZipWithList("pair2", bools, chars, results3, false);

        results3 = zipWithList(pair2, bools, chars, true);
        printSampleZipWithList("pair2", bools, chars, results3, true);

        List<ImmutablePair<Integer, Boolean>> results4;
        results4 = zipWithList(pair3, ints1, bools);
        printSampleZipWithList("pair3", ints1, bools, results4, false);

        results4 = zipWithList(pair3, ints1, bools, true);
        printSampleZipWithList("pair3", ints1, bools, results4, true);
    }

    public static <L, R, T> void printSampleZipWithList(final String funcName, final List<L> leftList, final List<R> rightList, final List<T> results, final Boolean isToAllowNull) {
        System.out.println();
        System.out.println("Allow Null  : " + isToAllowNull);
        System.out.println("Function    : " + funcName);
        System.out.println("Left        : " + leftList);
        System.out.println("Right       : " + rightList);
        System.out.println("ZipWithList : " + results);
    }
}
