package utilities;

import java.util.*;
import java.util.stream.*;

public class CollectionToStream {

    public static <T> Stream<T> transformToStreamOfObjects(final Collection<T> collection) {
        if (Objects.isNull(collection)) {
            return Stream.empty();
        }
        return collection
                .stream()
                .filter(Objects::nonNull);
    }

    public static <K, V> Stream<Map.Entry<K, V>> transformToStreamOfObjects(final Map<K, V> map) {
        return Optional.ofNullable(map)
                       .orElse(Collections.emptyMap())
                       .entrySet()
                       .stream()
                       .filter(Objects::nonNull);
    }

    public static void main(String[] args) {
        testStream();
    }

    public static void testStream() {
        List<String> listOfStrings = null;
        printStream(listOfStrings,
                transformToStreamOfObjects(listOfStrings)
                        .collect(Collectors.toList())
        );
        listOfStrings = Arrays.asList("a", "b", null, "c", "d", "a", "b");
        printStream(listOfStrings,
                transformToStreamOfObjects(listOfStrings)
                        .collect(Collectors.toList())
        );

        Set<String> setOfStrings = null;
        printStream(setOfStrings,
                transformToStreamOfObjects(setOfStrings)
                        .collect(Collectors.toSet())
        );
        setOfStrings = new HashSet<>(listOfStrings);
        printStream(setOfStrings,
                transformToStreamOfObjects(setOfStrings)
                        .collect(Collectors.toSet())
        );

        Map<String, String> mapOfStrings = null;
        Map<String, String> results =
                transformToStreamOfObjects(mapOfStrings)
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        printStream(mapOfStrings, results);
        mapOfStrings = new HashMap<>();
        mapOfStrings.put("a", "a");
        mapOfStrings.put("b", "b");
        mapOfStrings.put("c", "c");
        mapOfStrings.put("d", "d");
        results =
                transformToStreamOfObjects(mapOfStrings)
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        printStream(mapOfStrings, results);
    }

    public static <T> void printStream(final Collection<T> collection, final Collection<T> results) {
        System.out.println();
        System.out.println("Class      : " + results.getClass().getName());
        System.out.println("Collection : " + collection);
        System.out.println("Results    : " + results);
    }

    public static <K, V> void printStream(final Map<K, V> map, final Map<K, V> results) {
        System.out.println();
        System.out.println("Class   : " + results.getClass().getName());
        System.out.println("Map     : " + map);
        System.out.println("Results : " + results);
    }
}
