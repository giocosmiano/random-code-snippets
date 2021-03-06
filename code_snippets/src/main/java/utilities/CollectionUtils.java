package utilities;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

import org.apache.commons.lang3.tuple.ImmutablePair;

import static java.util.Optional.*;

public class CollectionUtils {
    private CollectionUtils() { }

    public static <T> Stream<T> transformToStreamOfObjects(final Collection<T> collection) {
        return Optional.ofNullable(collection)
                       .map(e -> e.stream().filter(Objects::nonNull))
                       .orElse(Stream.empty());
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

    public static <T> Optional<T> getMaybeHead(final Collection<T> list) {
        return transformToStreamOfObjects(list)
                .findFirst();
    }

    public static <T> T getHead(final Collection<T> list) {
        return getMaybeHead(list)
                .orElse(null);
    }

    public static <T> List<T> getTail(final List<T> list) {
        return ofNullable(list)
                .map(e -> transformToStreamOfObjects(e).skip(1).collect(Collectors.toList()))
                .orElse(Collections.emptyList());
    }

    public static <T> ImmutablePair<T, List<T>> getHeadAndTail(final Collection<T> list) {
        return of( transformToStreamOfObjects(list).collect(Collectors.toList()) )
                .filter(e -> ! e.isEmpty())
                .map(e -> {
                    T head = getHead(e);
                    List<T> tail = getTail(e);
                    return ImmutablePair.of(head, tail);
                })
                .orElse(null);
    }

/**
 * Running some experiments
    public static <T> Stream<T> streamOfObjects(final Stream<T> stream) {
        return ofNullable(stream)
                .map(e -> e.filter(Objects::nonNull))
                .orElse(Stream.empty());
    }

    public static <T> Optional<T> getHead(final Stream<T> stream) {
        return ofNullable(stream)
                .map(e -> e.filter(Objects::nonNull))
                .flatMap(Stream::findFirst);
    }

    public static <T> Stream<T> getTail(final Stream<T> stream) {
        return ofNullable(stream)
                .map(e -> e.filter(Objects::nonNull))
                .map(e -> e.skip(1))
                .orElse(Stream.empty());
    }

    public static <T> Optional<ImmutablePair<Optional<T>, Stream<T>>> getHeadAndTail(final Stream<T> stream) {
        return ofNullable(stream)
                .map(e -> e.filter(Objects::nonNull))
                .flatMap(e ->
                        ofNullable(getHead(e))
                                .map(e1 -> ImmutablePair.of(e1, getTail(e)))
                );
    }
*/

    public static <T> List<T> zipListToList(final List<List<T>> listOfList) {
        final Integer maxSize =
                transformToStreamOfObjects(listOfList)
                        .map(List::size)
                        .max(Integer::compare)
                        .orElse(0);

        return IntStream.range(0, maxSize)
                        .mapToObj(i ->
                                transformToStreamOfObjects(listOfList)
                                        .filter(e -> i < e.size())
                                        .map(e -> e.get(i))
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
