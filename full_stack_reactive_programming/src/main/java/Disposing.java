import io.reactivex.Flowable;
import io.reactivex.disposables.Disposable;

import java.util.concurrent.TimeUnit;

public class Disposing {
  public static void main(String[] args) throws InterruptedException {
    Disposable disposable = Flowable.interval(1, 1, TimeUnit.SECONDS)
        .map(index -> transform(index))
        .subscribe(System.out::println,
            err -> System.out.println("ERROR: " + err),
            () -> System.out.println("DONE"));

    Thread.sleep(5000);

    System.out.println("disposing");
    disposable.dispose();

    Thread.sleep(10000);
  }

  private static long transform(long index) {
    return index;
  }
}