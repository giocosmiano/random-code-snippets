import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

public class HotVsColdReactorFlux {

  private final AtomicInteger subscriber1 = new AtomicInteger();
  private final AtomicInteger subscriber2 = new AtomicInteger();
  private final AtomicInteger subscriber3 = new AtomicInteger();

  private final Function<Integer, Boolean> isPrime = number -> {
    for (int i = 2; i < number; i++) {
      if (number % i == 0) return false;
    }
    return true;
  };

  private final Function<Integer, Integer> getNextPrime = number -> {
    Integer iNbr = number;
    iNbr++;
    while (! isPrime.apply(iNbr)) iNbr++;
    return iNbr;
  };

  public static void main(String[] args) {
    HotVsColdReactorFlux observables = new HotVsColdReactorFlux();
    observables.runObservable(false);
  }

  public void doOnNext(boolean isHotObservable, final Integer subscriberNbr, final Integer data) {
    System.out.println(
//            String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - from doOnNext()"
            String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
                    , subscriberNbr == 1 ? String.format("Subscriber 1: %s", data) : ""
                    , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", data) : ""
                    , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", data) : ""
            ));
  }

  public Integer doubleTheValue(final Integer subscriberNbr, final Integer data) {
    final Integer doubleIt = data * 2;
    switch (subscriberNbr) {
      case 1: subscriber1.set(doubleIt);
      case 2: subscriber2.set(doubleIt);
      case 3: subscriber3.set(doubleIt);
    }
    return doubleIt;
  }

  public void printSubscribeMessage(boolean isHotObservable, final Integer subscriberNbr) {
    // do nothing
  }

  public void printSubscribeMessageX(boolean isHotObservable, final Integer subscriberNbr) {
    System.out.println(
            String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - from printSubscribeMessage()"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
                    , subscriberNbr == 1 ? String.format("Subscriber 1: %s", subscriber1.get()) : ""
                    , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", subscriber2.get()) : ""
                    , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", subscriber3.get()) : ""
            ));
  }

  public void setTimeout(final Integer millis) {
    try { Thread.sleep(millis); } catch (Exception e) {}
  }

  public void nextPrime(final Integer number, final FluxSink<Integer> observer) {
    final Integer prime = getNextPrime.apply(number);
    observer.next(prime);
    CompletableFuture.supplyAsync(() -> {
      setTimeout(100);
      nextPrime(prime, observer);
      return prime;
    });
  }

  public void runObservable(boolean isHotObservable) {
    System.out.println(
            String.format("Starting %s Reactor Flux from %s"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
            ));

    Flux<Integer> observable = Flux.create(observer -> nextPrime(1, observer));

    if (isHotObservable) observable = observable.share();

    Disposable disposable1 =
            observable.doOnNext(data -> doOnNext(isHotObservable,1, data))
                    .map(data -> doubleTheValue(1, data))
                    .subscribe(data -> printSubscribeMessage(isHotObservable,1));
    setTimeout(2000);

    Disposable disposable2 =
            observable.doOnNext(data -> doOnNext(isHotObservable,2, data))
                    .map(data -> doubleTheValue(2, data))
                    .subscribe(data -> printSubscribeMessage(isHotObservable,2));
    setTimeout(3000);

    Disposable disposable3 =
            observable.doOnNext(data -> doOnNext(isHotObservable,3, data))
                    .map(data -> doubleTheValue(3, data))
                    .subscribe(data -> printSubscribeMessage(isHotObservable,3));
    setTimeout(4000);

    disposable1.dispose();
    disposable2.dispose();
    disposable3.dispose();

    setTimeout(5000);
    System.out.println(
            String.format("DONE with %s Reactor Flux from %s"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
            ));
  }
}