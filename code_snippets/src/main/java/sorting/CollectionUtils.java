package sorting;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.*;
import java.util.stream.*;

import static java.util.Optional.*;

public class CollectionUtils {
    private CollectionUtils() { }

    public static <T> Stream<T> streamOfObjects(final List<T> list) {
        return ofNullable(list)
                .map(e -> e.stream().filter(Objects::nonNull))
                .orElse(Stream.empty());
    }

    public static <T> List<T> listOfObjects(final List<T> list) {
        return ofNullable(list)
                .map(CollectionUtils::streamOfObjects)
                .map(e -> e.collect(Collectors.toList()))
                .orElse(Collections.emptyList());
    }

    public static <T> Optional<T> getMaybeHead(final List<T> list) {
        return streamOfObjects(list)
                .findFirst();
    }

    public static <T> T getHead(final List<T> list) {
        return getMaybeHead(list)
                .orElse(null);
    }

    public static <T> List<T> getTail(final List<T> list) {
        return ofNullable(list)
                .map(e -> streamOfObjects(e).skip(1).collect(Collectors.toList()))
                .orElse(Collections.emptyList());
    }

    public static <T> ImmutablePair<T, List<T>> getHeadAndTail(final List<T> list) {
        return of( listOfObjects(list) )
                .filter(e -> ! e.isEmpty())
                .map(e -> {
                    T head = getHead(e);
                    List<T> tail = getTail(e);
                    return ImmutablePair.of(head, tail);
                })
                .orElse(null);
    }
}