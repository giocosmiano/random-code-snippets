package wordCount;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.of;

public class WordCount {

  public static void main(String[] args) {

    String text = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.";
    TreeMap<String, Integer> sortedMap =
            Arrays.stream(
                    of(text)
                            .orElse("")
                            .toLowerCase()
                            .split("\\s"))
                  .filter(e -> Objects.nonNull(e) && e.length() > 0 && ! e.chars().allMatch(Character::isWhitespace))
                  .map(e -> e.replaceAll("\\W", ""))
                  .collect(Collectors.groupingBy(Function.identity()))
                  .entrySet()
                  .stream()
//                  .sorted(Map.Entry.comparingByKey())  // no need as it returns a TreeMap sorted by keys
                  .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size(),
                          (oldValue, newValue) -> oldValue, TreeMap::new))
            ;
    System.out.println(sortedMap);
  }
}