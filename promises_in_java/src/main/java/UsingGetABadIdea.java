import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class UsingGetABadIdea {
  public static void main(String[] args) throws ExecutionException, InterruptedException {
    System.out.println("In start of main " + Thread.currentThread());

    final CompletableFuture<Integer> resultFuture = startAsyncTask(2);

    System.out.println("At the end of main " + Thread.currentThread());

    System.out.println(resultFuture.get()); //bad idea, blocking call and why bother.
  }

  private static CompletableFuture<Integer> startAsyncTask(int number) {
    return CompletableFuture.supplyAsync(() -> {
      sleep(2000);

      return number * 2;
    });
  }

  private static boolean sleep(int ms) {
    try {
      Thread.sleep(ms);
      return true;
    } catch (InterruptedException e) {
      return false;
    }
  }
}