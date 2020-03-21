package com.giocosmiano.exploration.reactiveApis;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;
import io.vavr.control.Either;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
@Component
public class HotVsColdObservables extends HotVsColdReactiveApis {

    private static final Logger log = LoggerFactory.getLogger(HotVsColdObservables.class);

    public static void main(String[] args) {
        log.info(
                String.format("Starting %s Observables from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));

        HotVsColdObservables observables = new HotVsColdObservables();
        observables.runObservable(DEFAULT_COLD_OBSERVABLE, DEFAULT_THRESHOLD);

        boolean anySubscribersStillListening;
        do {
            anySubscribersStillListening =
                    (
                            (observables.disposable1 != null && ! observables.disposable1.isDisposed())
                                    || (observables.disposable2 != null && ! observables.disposable2.isDisposed())
                                    || (observables.disposable3 != null && ! observables.disposable3.isDisposed())
                    );
            setTimeout.accept(1000);
        } while (anySubscribersStillListening);

        log.info(
                String.format("DONE with %s Observables from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));
    }

    private void nextPrime(final Integer number, final Integer threshold, final ObservableEmitter<Integer> observer) {
        final Integer prime = getNextPrime.apply(number);

        // Emit a completion when threshold is reached
        if (prime >= threshold) {
            CompletableFuture.runAsync(() -> {
                setTimeout.accept(500);
                observer.onComplete();
            });

            // https://github.com/ReactiveX/RxJava/wiki/Error-Handling
            // https://github.com/ReactiveX/RxJava/wiki/What's-different-in-2.0#error-handling
            // un-comment to simulate an `onError` and `tryOnError` that will halt the entire stream of data
//        } else if (prime >= 200) {
//            observer.onError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime));
//            observer.tryOnError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime));

            // emit the next data
        } else if (! observer.isDisposed()) {
            observer.onNext(prime);

            CompletableFuture.supplyAsync(() -> {
                setTimeout.accept(100);
                nextPrime(prime, threshold, observer);
                return prime;
            });
        }
    }

    private Observable<CompletableFuture<Either<String,Integer>>> createObservable(boolean isHotObservable, final Integer threshold) {
        Observable<CompletableFuture<Either<String,Integer>>> observable =
                Observable.<Integer>create(observer -> nextPrime(1, threshold, observer))
                        .switchMap(prime -> {

                            CompletableFuture<Either<String,Integer>> cf =
                                    CompletableFuture.supplyAsync(() -> {
                                        setTimeout.accept(100);
                                        return Either.right(prime);
                                    });
                            Observable<CompletableFuture<Either<String,Integer>>> disposableStream$ = Observable.just(cf);
                            return disposableStream$;
                        })
                ;

        if (isHotObservable) observable = observable.share();

        return observable;
    }

    // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
    // http://localhost:8080/getEmployeeDetails/123
    // CompletableFuture.supplyAsync(() -> getEmployee(empId))
    //         .thenApply(emp -> getEmployeeDept(empId))
    //         .thenApply(emp -> getEmployeePay(empId));
    public Observable<CompletableFuture<Either<String,Integer>>> runObservable(boolean isHotObservable, final Integer threshold) {

        Observable<CompletableFuture<Either<String,Integer>>> observable = createObservable(isHotObservable, threshold);

        Function<Integer, Consumer<Disposable>> onSubscribe =
                subscriberNbr -> disposable -> {
                    if (subscriberNbr == 1) {
                        disposable1 = disposable;

                    } else if (subscriberNbr == 2) {
                        disposable2 = disposable;

                    } else if (subscriberNbr == 3) {
                        disposable3 = disposable;
                    }
                };

        observable
                .doOnSubscribe(disposable -> onSubscribe.apply(1).accept(disposable))
                .map(promise -> doubleIt.apply(1).apply(promise))
                .map(promise -> resetIt.apply(1).apply(promise))
                .subscribe(
                        promise -> onNext.apply(1).accept(promise)
                        , error -> onError.apply(1).accept(error)
                        , () -> onComplete.accept(1)
                )
        ;

        return observable;
    }
}