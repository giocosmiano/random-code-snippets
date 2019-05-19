import java.util.concurrent.CompletableFuture;

public class DealingWithExceptions {
  private static boolean generateException = true;
  private static boolean recoverFromException = false;

  public static void main(String[] args) throws InterruptedException {
    //Run this code multiple times to see the behavior, by changing the above fields
    //for different combinations

    //generateException       false    true               false           true
    //recoverFromException    true     true               false           false
    //output                  21       "a fiasco"; 0      21              "a fiaso"; "no point trying further"

    //When you leave a then or a exceptionally, you return a CompletableFuture.
    //If that resolves, go the next then in the chain.
    //If that rejects, go the next exceptionally in the chain.

    CompletableFuture<Integer> task = new CompletableFuture<>();

    task
      .thenApply(e -> e * 2)
      .thenApply(e -> e + 1)
      .exceptionally(throwable -> logFailure(throwable))
      .thenAccept(System.out::println)
      .exceptionally(throwable -> logFailure2(throwable));

    if(generateException)
      task.completeExceptionally(new RuntimeException("a fiasco"));
    else
      task.complete(10);
  }

  private static Integer logFailure(Throwable throwable) {
    System.out.println(throwable);
    if(recoverFromException)
      return 0;
    else
      throw new RuntimeException("no point trying further");
  }

  private static Void logFailure2(Throwable throwable) {
    logFailure(throwable);
    return null;
  }
}