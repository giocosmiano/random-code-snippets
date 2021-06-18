package utilities;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;

import static utilities.ZipList.*;
import static org.junit.jupiter.api.Assertions.*;

public class ZipListTest {

    List<Integer> ints;
    List<String> chars;
    List<Boolean> bools;
    List<String> empty;

    @BeforeEach
    void init() {
        ints = Arrays.asList(11, 12, 13, null, 14, 15);
        chars = Arrays.asList("a", "b", "c", "d", null);
        bools = Arrays.asList(true, false, null, true);
        empty = Collections.emptyList();
    }

    @Test
    public void zipListOneOfListIsEmptyTest() {
        List<ImmutablePair<String, Integer>> results;
        results = zipList(empty, ints);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty");

        results = zipList(empty, ints, true);
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
}
