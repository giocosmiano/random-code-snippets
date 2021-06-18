package utilities;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.function.*;

import static org.junit.jupiter.api.Assertions.*;
import static utilities.ZipWithList.*;

public class ZipWithListTest {

    BiFunction<Integer, Integer, Integer> sum = Integer::sum;
    BiFunction<Integer, Integer, Integer> product = (a, b) -> a * b;
    BiFunction<Integer, Integer, Integer> multiplyBy2ThenAdd = (a, b) -> 2 * a + b;
    BiFunction<Integer, String, ImmutablePair<Integer, String>> fPair1 = ImmutablePair::new;
    BiFunction<Boolean, String, ImmutablePair<Boolean, String>> fPair2 = ImmutablePair::new;
    BiFunction<Integer, Boolean, ImmutablePair<Integer, Boolean>> fPair3 = ImmutablePair::new;

    List<Integer> ints1;
    List<Integer> ints2;
    List<String> chars;
    List<Boolean> bools;
    List<Integer> empty;

    @BeforeEach
    void init() {
        ints1 = Arrays.asList(1,   2, null,  3,  4,  5, 6);
        ints2 = Arrays.asList(11, 12, 13, null, 14, 15);
        chars = Arrays.asList("a", "b", "c", "d", null);
        bools = Arrays.asList(true, false, null, true);
        empty = Collections.emptyList();
    }

    @Test
    public void zipWithListOneOfListIsEmptyTest() {
        List<Integer> results;
        results = zipWithList(sum, empty, ints1);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty");

        results = zipWithList(sum, empty, ints1, true);
        assertTrue(results.isEmpty(), () -> "Should be empty result list if one of list is empty, with isToAllowNull == true");
    }

    @Test
    public void zipWithListUsingSumOnIntegersTest() {
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
