import java.util.*;

import static java.util.stream.Collectors.*;

public class JoiningStrings {
  public static void main(String[] args) {
    List<String> names = Arrays.asList("Jane", "Bill", "Brenda");

    //To print comma separated values, we don't have to struggle with loops and if conditions.
    System.out.println(names.stream().collect(joining(", ")));
    //Jane, Bill, Brenda
  }
}
