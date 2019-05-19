import java.util.*;
import java.util.concurrent.CompletableFuture;

public class ThenCombine {
  public static void main(String[] args) {
    final CompletableFuture<Integer> task1 = CompletableFuture.supplyAsync(() -> 2);
    final CompletableFuture<Integer> task2 = CompletableFuture.supplyAsync(() -> 4);

    task1.thenCombine(task2, (value1, value2) -> value1 + value2)
      .thenAccept(System.out::println);
  }
}