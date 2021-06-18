package utilities;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class ZipList {

    /**
     * Implementation of haskell `zip` function, while optionally allowing `null` value elements
     * http://zvon.org/other/haskell/Outputprelude/zip_f.html
     * https://hoogle.haskell.org/?hoogle=zip
     */
    public static <L, R> List<ImmutablePair<L, R>> zipList(final List<L> leftList, final List<R> rightList) {
        return zipList(leftList, rightList, false);
    }

    public static <L, R> List<ImmutablePair<L, R>> zipList(final List<L> leftList, final List<R> rightList, final Boolean isToAllowNull) {
        final Predicate<ImmutablePair<L, R>> checkForNull =
                pair -> isToAllowNull || Optional.of(pair)
                                                 .map(p -> Objects.nonNull(p.getLeft()) && Objects.nonNull(p.getRight()))
                                                 .orElse(false);

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
                            return new ImmutablePair<>(left, right);
                        })
                        .filter(checkForNull)
                        .collect(Collectors.toList());
    }

    public static void main(String[] args) {
        runSampleZipList();
    }

    /**
     * @see ZipListTest
     */
    public static void runSampleZipList() {
        List<Integer> ints = Arrays.asList(11, 12, 13, null, 14, 15);
        List<String> chars = Arrays.asList("a", "b", "c", "d", null);
        List<Boolean> bools = Arrays.asList(true, false, null, true);
        List<String> empty = Collections.emptyList();

        List<ImmutablePair<String, Integer>> results1;
        results1 = zipList(empty, ints);
        printSampleZipList(empty, ints, results1, false);

        results1 = zipList(empty, ints, true);
        printSampleZipList(empty, ints, results1, true);

        List<ImmutablePair<Integer, String>> results2;
        results2 = zipList(ints, chars);
        printSampleZipList(ints, chars, results2, false);

        results2 = zipList(ints, chars, true);
        printSampleZipList(ints, chars, results2, true);

        List<ImmutablePair<Boolean, String>> results3;
        results3 = zipList(bools, chars);
        printSampleZipList(bools, chars, results3, false);

        results3 = zipList(bools, chars, true);
        printSampleZipList(bools, chars, results3, true);

        List<ImmutablePair<Integer, Boolean>> results4;
        results4 = zipList(ints, bools);
        printSampleZipList(ints, bools, results4, false);

        results4 = zipList(ints, bools, true);
        printSampleZipList(ints, bools, results4, true);
    }

    public static <L, R> void printSampleZipList(final List<L> leftList, final List<R> rightList, final List<ImmutablePair<L, R>> results, final Boolean isToAllowNull) {
        System.out.println();
        System.out.println("Allow Null : " + isToAllowNull);
        System.out.println("Left       : " + leftList);
        System.out.println("Right      : " + rightList);
        System.out.println("ZipList    : " + results);
    }
}
