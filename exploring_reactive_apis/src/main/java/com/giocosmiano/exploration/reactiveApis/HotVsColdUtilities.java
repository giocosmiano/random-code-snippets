package com.giocosmiano.exploration.reactiveApis;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.function.Consumer;
import java.util.function.Function;

public class HotVsColdUtilities {

    public static Boolean DEFAULT_COLD_OBSERVABLE = false;
    public static Integer DEFAULT_THRESHOLD = 500;
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