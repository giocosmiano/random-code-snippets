import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
public class HotVsColdObservables {

  private final AtomicInteger subscriber1 = new AtomicInteger();
  private final AtomicInteger subscriber2 = new AtomicInteger();
  private final AtomicInteger subscriber3 = new AtomicInteger();

  private Disposable disposable1 = null;
  private Disposable disposable2 = null;
  private Disposable disposable3 = null;

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
    HotVsColdObservables observables = new HotVsColdObservables();
    observables.runObservable(false);
  }

  public void doOnNext(boolean isHotObservable, final Integer subscriberNbr, final Integer data) {
    System.out.println(
            String.format("%s Observables from %s - \t%s\t%s\t%s\t - doOnNext()"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
                    , subscriberNbr == 1 ? String.format("Subscriber 1: %s", data) : ""
                    , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", data) : ""
                    , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", data) : ""
            ));

    switch (subscriberNbr) {
      case 1: subscriber1.set(data);
      case 2: subscriber2.set(data);
      case 3: subscriber3.set(data);
    }
  }

  public void doOnError(boolean isHotObservable, final Integer subscriberNbr, final Throwable error) {
    System.out.println(
            String.format("%s Observables from %s - \t%s\t%s\t%s\t - doOnError()"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
                    , subscriberNbr == 1 ? String.format("Subscriber 1: %s", error.getMessage()) : ""
                    , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", error.getMessage()) : ""
                    , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", error.getMessage()) : ""
            ));
  }

  public void doOnComplete(boolean isHotObservable, final Integer subscriberNbr) {
    System.out.println(
            String.format("%s Observables from %s - \t%s\t%s\t%s\t - doOnComplete()"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
                    , subscriberNbr == 1 ? String.format("Subscriber 1: %s", subscriber1.get()) : ""
                    , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", subscriber2.get()) : ""
                    , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", subscriber3.get()) : ""
            ));

    // NOTE: dispose() seems to un-subscribe all subscribers to an Observable rather than just the individual subscription
//    switch (subscriberNbr) {
//      case 1: disposable1.dispose();
//      case 2: disposable2.dispose();
//      case 3: disposable3.dispose();
//    }
  }

  public void setTimeout(final Integer millis) {
    try { Thread.sleep(millis); } catch (Exception e) {}
  }

  public void nextPrime(final Integer number, final ObservableEmitter<Integer> observer) {
    final Integer prime = getNextPrime.apply(number);

    if (prime >= 500) {
      observer.onComplete();

      // https://github.com/ReactiveX/RxJava/wiki/Error-Handling
      // https://github.com/ReactiveX/RxJava/wiki/What's-different-in-2.0#error-handling
      // un-comment to simulate an `onError` and `tryOnError` that will halt the entire stream of data
//    } else if (prime >= 200) {
//      observer.onError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime));
//      observer.tryOnError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime));
    }

    if (! observer.isDisposed()) {
      observer.onNext(prime);
    }

    CompletableFuture.supplyAsync(() -> {
      setTimeout(100);
      nextPrime(prime, observer);
      return prime;
    });
  }

  public void runObservable(boolean isHotObservable) {
    System.out.println(
            String.format("Starting %s Observables from %s"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
            ));

    Observable<Integer> observable = Observable.<Integer>create(observer -> nextPrime(1, observer))
            .switchMap(prime -> {
              Observable<Integer> disposableStream$ = Observable.just(prime);
              return disposableStream$
                      .map(data -> {
                        if (data >= 100 && data <= 200) {
                          throw new RuntimeException(String.format("Simulating an error skipping prime=%s, in-between 100 and 200, while continue streaming the rest", prime));
                        }
                        return data;
                      })
                      .onErrorReturn(error -> {
                        System.out.println(error.getMessage());
                        return 0;
                      });
            })
            ;

    if (isHotObservable) observable = observable.share();

    Function<Integer, Consumer<Integer>> onNext =
            subscriberNbr -> data -> doOnNext(isHotObservable, subscriberNbr, data);

    Function<Integer, Consumer<Throwable>> onError =
            subscriberNbr -> error -> doOnError(isHotObservable, subscriberNbr, error);

    Consumer<Integer> onComplete =
            subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr);

    Function<Integer, Consumer<Disposable>> onSubscribe =
            subscriberNbr -> disposable -> {
              switch (subscriberNbr) {
                case 1: disposable1 = disposable;
                case 2: disposable2 = disposable;
                case 3: disposable3 = disposable;
              }
            };

    /*
      un-comment the 2 implementations to see the difference on how exception handling is perform between
      1) .subscribe(onData, onError, onComplete) vs
      2) .doOnNext, .doOnError, .doOnComplete
      https://github.com/ReactiveX/RxJava/issues/6163
     */
    observable
            .doOnSubscribe(disposable -> onSubscribe.apply(1).accept(disposable))
            .subscribe(
                    data -> onNext.apply(1).accept(data)
                    , error -> onError.apply(1).accept(error)
                    , () -> onComplete.accept(1)
            );
//    observable
//            .doOnNext(data -> onNext.apply(1).accept(data))
//            .doOnError(error -> onError.apply(1).accept(error))
//            .doOnComplete(() -> onComplete.accept(1))
//            .doOnSubscribe(disposable -> onSubscribe.apply(1).accept(disposable))
//            .onErrorResumeNext(observable)
//            .subscribe();

    setTimeout(2000);
    observable
            .doOnSubscribe(disposable -> onSubscribe.apply(2).accept(disposable))
            .subscribe(
                    data -> onNext.apply(2).accept(data)
                    , error -> onError.apply(2).accept(error)
                    , () -> onComplete.accept(2)
            );
//    observable
//            .doOnNext(data -> onNext.apply(2).accept(data))
//            .doOnError(error -> onError.apply(2).accept(error))
//            .doOnComplete(() -> onComplete.accept(2))
//            .doOnSubscribe(disposable -> onSubscribe.apply(2).accept(disposable))
//            .onErrorResumeNext(observable)
//            .subscribe();

    setTimeout(2000);
    observable
            .doOnSubscribe(disposable -> onSubscribe.apply(3).accept(disposable))
            .subscribe(
                    data -> onNext.apply(3).accept(data)
                    , error -> onError.apply(3).accept(error)
                    , () -> onComplete.accept(3)
            );
//    observable
//            .doOnNext(data -> onNext.apply(3).accept(data))
//            .doOnError(error -> onError.apply(3).accept(error))
//            .doOnComplete(() -> onComplete.accept(3))
//            .doOnSubscribe(disposable -> onSubscribe.apply(3).accept(disposable))
//            .onErrorResumeNext(observable)
//            .subscribe();

    setTimeout(10000);
    System.out.println(
            String.format("DONE with %s Observables from %s"
                    , isHotObservable ? "Hot" : "Cold"
                    , Thread.currentThread().getName()
            ));
  }
}