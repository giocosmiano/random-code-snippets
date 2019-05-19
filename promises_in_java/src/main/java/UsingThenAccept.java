import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class UsingThenAccept {
  public static void main(String[] args) throws ExecutionException, InterruptedException {
    startAsyncTask(2)
      .thenAccept(result -> System.out.println(result));
  }

  private static CompletableFuture<Integer> startAsyncTask(int number) {
    return CompletableFuture.supplyAsync(() -> {
      if(Math.random() > 0.75) {
        System.out.println("taking a slow run this time");
        sleep(2000);
      }

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