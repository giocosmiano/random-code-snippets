package utilities;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static utilities.ZipListToList.*;

public class ZipListToListTest {

    List<Integer> ints0;
    List<Integer> ints1;
    List<Integer> ints2;
    List<Integer> ints3;
    List<Integer> empty;

    @BeforeEach
    void init() {
        ints0 = null;
        ints1 = Arrays.asList(11, 12, 13, 14, null, 15);
        ints2 = Arrays.asList(21, 22, null, 23, 24);
        ints3 = Arrays.asList(31, null, 32, 33);
        empty = Collections.emptyList();
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
}
