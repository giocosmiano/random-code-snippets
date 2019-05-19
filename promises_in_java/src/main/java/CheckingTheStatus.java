import java.util.concurrent.CompletableFuture;

public class CheckingTheStatus {
  public static void main(String[] args) throws InterruptedException {
    final CompletableFuture<Integer> task = createTask();

    Thread.sleep(100);

    System.out.println(task.isDone());
    System.out.println(task.isCancelled());
    System.out.println(task.isCompletedExceptionally());
  }

  private static CompletableFuture<Integer> createTask() {
    //return CompletableFuture.supplyAsync(() -> 2);
    return CompletableFuture.supplyAsync(() -> { throw new RuntimeException("oops"); });
  }
}