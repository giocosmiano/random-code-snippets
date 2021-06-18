package utilities;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.stream.*;

import static utilities.CollectionToStream.*;
import static org.junit.jupiter.api.Assertions.*;

public class CollectionToStreamTest {
    Set<String> setOfStrings;
    List<String> listOfStrings;
    Collection<String> emptyStrings;
    Map<Integer, String> mapOfIntegerAndStrings;
    Map<Integer, String> emptyMap;

    @BeforeEach
    void init() {
        emptyStrings = null;
        emptyMap = null;
        listOfStrings = Arrays.asList("a", "b", null, "c", "d", "a", "b");
        setOfStrings = new HashSet<>(listOfStrings);
        mapOfIntegerAndStrings = new HashMap<>();
        mapOfIntegerAndStrings.put(1, "a");
        mapOfIntegerAndStrings.put(2, "b");
        mapOfIntegerAndStrings.put(3, "c");
        mapOfIntegerAndStrings.put(4, "d");
        mapOfIntegerAndStrings.put(null, "c");
        mapOfIntegerAndStrings.put(6, null);
        mapOfIntegerAndStrings.put(null, null);
        mapOfIntegerAndStrings.put(7, "g");
    }

    @Test
    public void collectionIsEmptyTest() {
        List<String> results = transformToStreamOfObjects(emptyStrings).collect(Collectors.toList());
        assertTrue(results.isEmpty(), () -> "Should be empty result list");
    }

    @Test
    public void collectionToStreamTest() {
        List<String> expected1 = Arrays.asList(
                listOfStrings.get(0),
                listOfStrings.get(1),
                listOfStrings.get(3),
                listOfStrings.get(4),
                listOfStrings.get(5),
                listOfStrings.get(6)
        );
        List<String> results = transformToStreamOfObjects(listOfStrings).collect(Collectors.toList());
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 6, () -> "Should have 6 elements");

        Set<String> expected2 = new HashSet<>();
        expected2.add(listOfStrings.get(0));
        expected2.add(listOfStrings.get(1));
        expected2.add(listOfStrings.get(3));
        expected2.add(listOfStrings.get(4));
        results = transformToStreamOfObjects(setOfStrings).collect(Collectors.toList());
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 4, () -> "Should have 4 elements");
    }

    @Test
    public void mapIsEmptyTest() {
        Map<Integer, String> results =
                transformToStreamOfObjects(emptyMap)
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        assertTrue(results.isEmpty(), () -> "Should be empty result map");
    }

    @Test
    public void mapToStreamTest() {
        Map<Integer, String> expected1 = new HashMap<>();
        mapOfIntegerAndStrings.forEach((k, v) -> {
            if (Objects.nonNull(k)) {
                expected1.put(k, v);
            }
        });

        Map<Integer, String> expected2 = new HashMap<>();
        mapOfIntegerAndStrings.forEach(expected2::put);

        // work around for known bug in openJDK
        // https://stackoverflow.com/questions/24630963/java-8-nullpointerexception-in-collectors-tomap
        // https://bugs.openjdk.java.net/browse/JDK-8148463
//        Map<Integer, String> results =
//                transformToStreamOfObjects(mapOfIntegerAndStrings)
//                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (existing, replacement) -> existing));
        Map<Integer, String> results =
                transformToStreamOfObjects(mapOfIntegerAndStrings)
                        .collect(HashMap::new, (m,v) -> m.put(v.getKey(), v.getValue()), HashMap::putAll);
        assertIterableEquals(expected1.entrySet(), results.entrySet());
        assertEquals(results.size(), 6, () -> "Should have 6 elements");

        results =
                transformToStreamOfObjects(mapOfIntegerAndStrings, true)
                        .collect(HashMap::new, (m,v) -> m.put(v.getKey(), v.getValue()), HashMap::putAll);
        assertIterableEquals(expected2.entrySet(), results.entrySet());
        assertEquals(results.size(), 7, () -> "Should have 7 elements");
    }
}
