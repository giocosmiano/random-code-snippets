package utilities;

import org.apache.commons.lang3.tuple.ImmutablePair;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.*;

import static utilities.CollectionUtils.*;
import static org.junit.jupiter.api.Assertions.*;

public class CollectionUtilsTest {
    Set<String> setOfStrings;
    List<String> listOfStrings;
    Collection<String> emptyStrings;
    Map<Integer, String> mapOfIntegerAndStrings;
    Map<Integer, String> emptyMap;

    List<Integer> ints0;
    List<Integer> ints1;
    List<Integer> ints2;
    List<Integer> ints3;
    List<Integer> empty;

    List<Integer> ints;
    List<String> chars;
    List<Boolean> bools;

    BiFunction<Integer, Integer, Integer> sum = Integer::sum;
    BiFunction<Integer, Integer, Integer> product = (a, b) -> a * b;
    BiFunction<Integer, Integer, Integer> multiplyBy2ThenAdd = (a, b) -> 2 * a + b;
    BiFunction<Integer, String, ImmutablePair<Integer, String>> fPair1 = ImmutablePair::new;
    BiFunction<Boolean, String, ImmutablePair<Boolean, String>> fPair2 = ImmutablePair::new;
    BiFunction<Integer, Boolean, ImmutablePair<Integer, Boolean>> fPair3 = ImmutablePair::new;

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

        ints0 = null;
        ints1 = Arrays.asList(11, 12, 13, 14, null, 15);
        ints2 = Arrays.asList(21, 22, null, 23, 24);
        ints3 = Arrays.asList(31, null, 32, 33);
        empty = Collections.emptyList();

        ints = Arrays.asList(11, 12, 13, null, 14, 15);
        chars = Arrays.asList("a", "b", "c", "d", null);
        bools = Arrays.asList(true, false, null, true);
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

    @Test
    public void zipListToListEmptyTest() {
        List<List<Integer>> listOfList = null;
        List<Integer> results = zipListToList(listOfList);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if list of list is empty");

        listOfList = Collections.emptyList();
        results = zipListToList(listOfList);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if list of list is empty");
    }

    @Test
    public void zipListToListTest() {
        List<Integer> expected1 =
                Arrays.asList(
                        ints1.get(0), ints2.get(0), ints3.get(0),
                        ints1.get(1), ints2.get(1),
                        ints1.get(2), ints3.get(2),
                        ints1.get(3), ints2.get(3), ints3.get(3),
                        ints2.get(4),
                        ints1.get(5)
                );

        List<Integer> expected2 =
                Arrays.asList(
                        ints3.get(0), ints2.get(0), ints1.get(0),
                        ints2.get(1), ints1.get(1),
                        ints3.get(2), ints1.get(2),
                        ints3.get(3), ints2.get(3), ints1.get(3),
                        ints2.get(4),
                        ints1.get(5)
                );

        List<List<Integer>> listOfList = Arrays.asList(ints1, ints0, ints2, ints3);
        List<Integer> results = zipListToList(listOfList);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 12, () -> "Should have 12 elements");

        listOfList = Arrays.asList(ints3, ints0, ints2, ints1);
        results = zipListToList(listOfList);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 12, () -> "Should have 12 elements");
    }

    @Test
    public void zipListOneOfListIsEmptyTest() {
        List<String> empty1 = Collections.emptyList();
        List<ImmutablePair<String, Integer>> results;
        results = zipList(empty1, ints);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty");

        results = zipList(empty1, ints, true);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty, with isToAllowNull == true");
    }

    @Test
    public void zipListIntegerAndStringTest() {
        ImmutablePair<Integer, String> pair0 = new ImmutablePair<>(ints.get(0), chars.get(0));
        ImmutablePair<Integer, String> pair1 = new ImmutablePair<>(ints.get(1), chars.get(1));
        ImmutablePair<Integer, String> pair2 = new ImmutablePair<>(ints.get(2), chars.get(2));
        ImmutablePair<Integer, String> pair3 = new ImmutablePair<>(ints.get(3), chars.get(3));
        ImmutablePair<Integer, String> pair4 = new ImmutablePair<>(ints.get(4), chars.get(4));
        List<ImmutablePair<Integer, String>> expected1 = Arrays.asList(pair0, pair1, pair2);
        List<ImmutablePair<Integer, String>> expected2 = Arrays.asList(pair0, pair1, pair2, pair3, pair4);

        List<ImmutablePair<Integer, String>> results;
        results = zipList(ints, chars);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 3, () -> "Should have 3-pairs of <Integer, String> elements");

        results = zipList(ints, chars, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 5, () -> "Should have 5-pairs of <Integer, String> elements, with isToAllowNull == true");
    }

    @Test
    public void zipListBoolAndStringTest() {
        ImmutablePair<Boolean, String> pair0 = new ImmutablePair<>(bools.get(0), chars.get(0));
        ImmutablePair<Boolean, String> pair1 = new ImmutablePair<>(bools.get(1), chars.get(1));
        ImmutablePair<Boolean, String> pair2 = new ImmutablePair<>(bools.get(2), chars.get(2));
        ImmutablePair<Boolean, String> pair3 = new ImmutablePair<>(bools.get(3), chars.get(3));
        List<ImmutablePair<Boolean, String>> expected1 = Arrays.asList(pair0, pair1, pair3);
        List<ImmutablePair<Boolean, String>> expected2 = Arrays.asList(pair0, pair1, pair2, pair3);

        List<ImmutablePair<Boolean, String>> results;
        results = zipList(bools, chars);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 3, () -> "Should have 3-pairs of <Boolean, String> elements");

        results = zipList(bools, chars, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 4, () -> "Should have 4-pairs of <Boolean, String> elements, with isToAllowNull == true");
    }

    @Test
    public void zipListIntegerAndBoolTest() {
        ImmutablePair<Integer, Boolean> pair0 = new ImmutablePair<>(ints.get(0), bools.get(0));
        ImmutablePair<Integer, Boolean> pair1 = new ImmutablePair<>(ints.get(1), bools.get(1));
        ImmutablePair<Integer, Boolean> pair2 = new ImmutablePair<>(ints.get(2), bools.get(2));
        ImmutablePair<Integer, Boolean> pair3 = new ImmutablePair<>(ints.get(3), bools.get(3));
        List<ImmutablePair<Integer, Boolean>> expected1 = Arrays.asList(pair0, pair1);
        List<ImmutablePair<Integer, Boolean>> expected2 = Arrays.asList(pair0, pair1, pair2, pair3);

        List<ImmutablePair<Integer, Boolean>> results;
        results = zipList(ints, bools);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 2, () -> "Should have 2-pairs of <Integer, Boolean> elements");

        results = zipList(ints, bools, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 4, () -> "Should have 4-pairs of <Integer, Boolean> elements, with isToAllowNull == true");
    }

    @Test
    public void zipWithListOneOfListIsEmptyTest() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        empty = Collections.emptyList();

        List<Integer> results;
        results = zipWithList(sum, empty, ints1);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty");

        results = zipWithList(sum, empty, ints1, true);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty, with isToAllowNull == true");
    }

    @Test
    public void zipWithListUsingSumOnIntegersTest() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        ints2 = Arrays.asList(11, 12, 13, null, 14, 15);

        List<Integer> expected1 = Arrays.asList(
                sum.apply(ints1.get(0), ints2.get(0)),
                sum.apply(ints1.get(1), ints2.get(1)),
                sum.apply(ints1.get(4), ints2.get(4)),
                sum.apply(ints1.get(5), ints2.get(5))
        );
        List<Integer> expected2 = Arrays.asList(
                sum.apply(ints1.get(0), ints2.get(0)),
                sum.apply(ints1.get(1), ints2.get(1)),
                null, null,
                sum.apply(ints1.get(4), ints2.get(4)),
                sum.apply(ints1.get(5), ints2.get(5))
        );

        List<Integer> results = zipWithList(sum, ints1, ints2);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 4, () -> "Should have 4 elements");

        results = zipWithList(sum, ints1, ints2, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 6, () -> "Should have 4 elements, with isToAllowNull == true");
    }

    @Test
    public void zipWithListUsingProductOnIntegersTest() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        ints2 = Arrays.asList(11, 12, 13, null, 14, 15);

        List<Integer> expected1 = Arrays.asList(
                product.apply(ints1.get(0), ints2.get(0)),
                product.apply(ints1.get(1), ints2.get(1)),
                product.apply(ints1.get(4), ints2.get(4)),
                product.apply(ints1.get(5), ints2.get(5))
        );
        List<Integer> expected2 = Arrays.asList(
                product.apply(ints1.get(0), ints2.get(0)),
                product.apply(ints1.get(1), ints2.get(1)),
                null, null,
                product.apply(ints1.get(4), ints2.get(4)),
                product.apply(ints1.get(5), ints2.get(5))
        );

        List<Integer> results = zipWithList(product, ints1, ints2);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 4, () -> "Should have 4 elements");

        results = zipWithList(product, ints1, ints2, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 6, () -> "Should have 4 elements, with isToAllowNull == true");
    }

    @Test
    public void zipWithListUsingMultiplyBy2ThenAddOnIntegersTest() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        ints2 = Arrays.asList(11, 12, 13, null, 14, 15);

        List<Integer> expected1 = Arrays.asList(
                multiplyBy2ThenAdd.apply(ints1.get(0), ints2.get(0)),
                multiplyBy2ThenAdd.apply(ints1.get(1), ints2.get(1)),
                multiplyBy2ThenAdd.apply(ints1.get(4), ints2.get(4)),
                multiplyBy2ThenAdd.apply(ints1.get(5), ints2.get(5))
        );
        List<Integer> expected2 = Arrays.asList(
                multiplyBy2ThenAdd.apply(ints1.get(0), ints2.get(0)),
                multiplyBy2ThenAdd.apply(ints1.get(1), ints2.get(1)),
                null, null,
                multiplyBy2ThenAdd.apply(ints1.get(4), ints2.get(4)),
                multiplyBy2ThenAdd.apply(ints1.get(5), ints2.get(5))
        );

        List<Integer> results = zipWithList(multiplyBy2ThenAdd, ints1, ints2);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 4, () -> "Should have 4 elements");

        results = zipWithList(multiplyBy2ThenAdd, ints1, ints2, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 6, () -> "Should have 4 elements, with isToAllowNull == true");
    }

    @Test
    public void zipWithListIntegerAndStringTest() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        chars = Arrays.asList("a", "b", "c", "d", null);

        ImmutablePair<Integer, String> pair0 = fPair1.apply(ints1.get(0), chars.get(0));
        ImmutablePair<Integer, String> pair1 = fPair1.apply(ints1.get(1), chars.get(1));
        ImmutablePair<Integer, String> pair2 = fPair1.apply(ints1.get(2), chars.get(2));
        ImmutablePair<Integer, String> pair3 = fPair1.apply(ints1.get(3), chars.get(3));
        ImmutablePair<Integer, String> pair4 = fPair1.apply(ints1.get(4), chars.get(4));
        List<ImmutablePair<Integer, String>> expected1 = Arrays.asList(pair0, pair1, pair3);
        List<ImmutablePair<Integer, String>> expected2 = Arrays.asList(pair0, pair1, null, pair3, null);

        List<ImmutablePair<Integer, String>> results;
        results = zipWithList(fPair1, ints1, chars);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 3, () -> "Should have 3-pairs of <Integer, String> elements");

        results = zipWithList(fPair1, ints1, chars, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 5, () -> "Should have 5-pairs of <Integer, String> elements, with isToAllowNull == true");
    }

    @Test
    public void zipWithListBooleanAndStringTest() {
        chars = Arrays.asList("a", "b", "c", "d", null);
        bools = Arrays.asList(true, false, null, true);

        ImmutablePair<Boolean, String> pair0 = fPair2.apply(bools.get(0), chars.get(0));
        ImmutablePair<Boolean, String> pair1 = fPair2.apply(bools.get(1), chars.get(1));
        ImmutablePair<Boolean, String> pair2 = fPair2.apply(bools.get(2), chars.get(2));
        ImmutablePair<Boolean, String> pair3 = fPair2.apply(bools.get(3), chars.get(3));
        List<ImmutablePair<Boolean, String>> expected1 = Arrays.asList(pair0, pair1, pair3);
        List<ImmutablePair<Boolean, String>> expected2 = Arrays.asList(pair0, pair1, null, pair3);

        List<ImmutablePair<Boolean, String>> results;
        results = zipWithList(fPair2, bools, chars);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 3, () -> "Should have 3-pairs of <Boolean, String> elements");

        results = zipWithList(fPair2, bools, chars, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 4, () -> "Should have 4-pairs of <Boolean, String> elements, with isToAllowNull == true");
    }

    @Test
    public void zipWithListIntegerAndBooleanTest() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        bools = Arrays.asList(true, false, null, true);

        ImmutablePair<Integer, Boolean> pair0 = fPair3.apply(ints1.get(0), bools.get(0));
        ImmutablePair<Integer, Boolean> pair1 = fPair3.apply(ints1.get(1), bools.get(1));
        ImmutablePair<Integer, Boolean> pair2 = fPair3.apply(ints1.get(2), bools.get(2));
        ImmutablePair<Integer, Boolean> pair3 = fPair3.apply(ints1.get(3), bools.get(3));
        List<ImmutablePair<Integer, Boolean>> expected1 = Arrays.asList(pair0, pair1, pair3);
        List<ImmutablePair<Integer, Boolean>> expected2 = Arrays.asList(pair0, pair1, null, pair3);

        List<ImmutablePair<Integer, Boolean>> results;
        results = zipWithList(fPair3, ints1, bools);
        assertIterableEquals(expected1, results);
        assertEquals(results.size(), 3, () -> "Should have 3-pairs of <Boolean, String> elements");

        results = zipWithList(fPair3, ints1, bools, true);
        assertIterableEquals(expected2, results);
        assertEquals(results.size(), 4, () -> "Should have 4-pairs of <Boolean, String> elements, with isToAllowNull == true");
    }
}
