/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package kidsSchoolProjects;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 *
 * @author gabriellecosmiano
 */
public class WordCount {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        String sample = "hello world,\n\t Hello There!!!";
        TreeMap<String, Integer> sortedMap =
                Arrays.stream(
                        Optional.ofNullable(sample)
                                .orElse("")
                                .toLowerCase()
                                .split("\\s"))
                        .filter(e -> Objects.nonNull(e) && e.length() > 0 && ! e.chars().allMatch(Character::isWhitespace))
                        .map(e -> e.replaceAll("\\W", ""))
                        .collect(Collectors.groupingBy(Function.identity()))
                        .entrySet()
                        .stream()
                        .sorted(Map.Entry.comparingByKey())
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size(),
                                (oldValue, newValue) -> oldValue, TreeMap::new))
                ;
        sortedMap.entrySet().forEach(System.out::println);
    }
}
