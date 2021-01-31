package utilities;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

import org.apache.commons.lang3.tuple.ImmutablePair;

public class CollectionUtils {
    private CollectionUtils() { }

    public static <T> Stream<T> transformToStreamOfObjects(final Collection<T> collection) {
        if (Objects.isNull(collection)) {
            return Stream.empty();
        }
        return collection
                .stream()
                .filter(Objects::nonNull);
    }

    public static <K, V> Stream<Map.Entry<K, V>> transformToStreamOfObjects(final Map<K, V> map) {
        return transformToStreamOfObjects(map, false);
    }

    public static <K, V> Stream<Map.Entry<K, V>> transformToStreamOfObjects(final Map<K, V> map, final Boolean isToAllowNull) {
        Predicate<Map.Entry<K, V>> checkForNullKey = e -> isToAllowNull || Objects.nonNull(e.getKey());
        return Optional.ofNullable(map)
                       .orElse(Collections.emptyMap())
                       .entrySet()
                       .stream()
                       .filter(Objects::nonNull)
                       .filter(checkForNullKey);
    }

    public static <T> List<T> zipListToList(final List<List<T>> listOfList) {
        final Integer maxSize =
                transformToStreamOfObjects(listOfList)
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
}
