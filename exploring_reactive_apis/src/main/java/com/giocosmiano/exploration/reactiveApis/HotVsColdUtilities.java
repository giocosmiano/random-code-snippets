package com.giocosmiano.exploration.reactiveApis;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.function.Consumer;
import java.util.function.Function;

public class HotVsColdUtilities {

    public static final Integer START_PRIME_AT_1 = 1;
    public static final Integer SUBSCRIBER_NBR_1 = 1;
    public static final Integer SUBSCRIBER_NBR_2 = 2;
    public static final Integer SUBSCRIBER_NBR_3 = 3;
    public static final Integer DEFAULT_THRESHOLD = 1200;
    public static final Boolean DEFAULT_COLD_OBSERVABLE = false;

    private static final Logger log = LoggerFactory.getLogger(HotVsColdUtilities.class);

    public static Function<Integer, Boolean> isPrime = number -> {
        for (int i = 2; i < number; i++) {
            if (number % i == 0) return false;
        }
        return true;
    };

    public static Function<Integer, Integer> getNextPrime = number -> {
        Integer iNbr = number + 1;
        while (! isPrime.apply(iNbr)) iNbr++;
        return iNbr;
    };

    public static Consumer<Integer> setTimeout = millis -> {
        try { Thread.sleep(millis); } catch (Exception e) { }
    };
}