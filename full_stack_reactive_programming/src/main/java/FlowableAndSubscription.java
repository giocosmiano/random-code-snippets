import io.reactivex.Flowable;

import java.util.concurrent.TimeUnit;

public class FlowableAndSubscription {
  public static void main(String[] args) throws InterruptedException {
    Flowable.interval(1, 1, TimeUnit.SECONDS)
        .map(index -> transform(index))
        .subscribe(System.out::println,
            err -> System.out.println("ERROR: " + err),
            () -> System.out.println("DONE"));

    Thread.sleep(10000);
  }

  private static long transform(long index) {
    System.out.println("transforming...");
    if(index == 5) {
      throw  new RuntimeException("oops, something went wrong");
    }
    return index;
  }
}