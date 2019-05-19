import java.util.*;
import java.util.concurrent.CompletableFuture;

public class ThenCompose {
  public static CompletableFuture<Integer> createTask(int input) {
    return CompletableFuture.supplyAsync(() -> input + 1);
  }

  public static void main(String[] args) {
    CompletableFuture.supplyAsync(() -> 2)
      .thenCompose(input -> createTask(input))
      .thenAccept(System.out::println);
  }
}

/*
Stream					CompletableFuture
map							thenApply
forEach					theAccept
reduce					thenCombine
flatMap         thenCompose

Stream
			func1  x -> y
			Stream<X>.map(func1) ==> Stream<Y>

			func2  x -> Stream<Y>
			Stream<X>.map(func2) ==> Stream<Stream<Y>>
			Stream<X>.flatMap(func2) ==> Stream<Y>


				func1  x - y
				CompletableFuture<X>.thenApply(func1) ==> CompletableFuture<Y>

				func2 x -> CompletableFuture<Y>
				CompletableFuture<X>.thenApply(func2) ==> CompletableFuture<CompletableFuture<Y>>

				CompletableFuture<X>.thenCompose(func2) ==> CompletableFuture<Y>
 */