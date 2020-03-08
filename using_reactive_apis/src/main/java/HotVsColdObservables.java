import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.disposables.Disposable;

import java.util.concurrent.atomic.AtomicInteger;

public class HotVsColdObservables {

  AtomicInteger subscriber1 = new AtomicInteger();
  AtomicInteger subscriber2 = new AtomicInteger();
  AtomicInteger subscriber3 = new AtomicInteger();

  public static void main(String[] args) {
    HotVsColdObservables observables = new HotVsColdObservables();
    observables.runObservable(true);
  }

  public void printMessage(boolean isHotObservable) {
    System.out.println(
            String.format("%s Observable - Subscriber1: %s\tSubscriber2: %s\tSubscriber3: %s"
                    , isHotObservable ? "Hot" : "Cold"
                    , subscriber1.get()
                    , subscriber2.get()
                    , subscriber3.get()
            ));
  }

  public boolean isPrime(final Integer nbr) {
    for (int i = 2; i < nbr; i++) {
      if (nbr % i == 0) return false;
    }
    return true;
  }

  public void setTimeout(final Integer millis) {
    try { Thread.sleep(millis); } catch (Exception e) {}
  }

  public void setTimeout(Runnable runnable, int millis) {
    new Thread(() -> {
      try {
        Thread.sleep(millis);
        runnable.run();
      } catch (Exception e) {}
    }).start();
  }

  public void nextPrime(final Integer number, final ObservableEmitter<Integer> observer) {
    Integer iNbr = number;
    iNbr++;
    while (! isPrime(iNbr)) iNbr++;
    final Integer oNbr = iNbr;
    observer.onNext(oNbr);
    setTimeout(() -> nextPrime(oNbr, observer), 100);
  }

  public void runObservable(boolean isHotObservable) {
    Observable<Integer> observable = Observable.create(observer -> nextPrime(1, observer));

    if (isHotObservable) observable = observable.share();

    Disposable disposable1 = observable.subscribe(data -> {
      subscriber1.set(data);
      printMessage(isHotObservable);
    });
    setTimeout(2000);

    Disposable disposable2 = observable.subscribe(data -> {
      subscriber2.set(data);
      printMessage(isHotObservable);
    });
    setTimeout(3000);

    Disposable disposable3 = observable.subscribe(data -> {
      subscriber3.set(data);
      printMessage(isHotObservable);
    });
    setTimeout(4000);

    disposable1.dispose();
    disposable2.dispose();
    disposable3.dispose();
  }
}