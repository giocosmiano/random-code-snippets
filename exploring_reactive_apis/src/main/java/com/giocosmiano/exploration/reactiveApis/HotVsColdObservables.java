package com.giocosmiano.exploration.reactiveApis;

import io.reactivex.Observable;
import io.reactivex.ObservableEmitter;
import io.reactivex.disposables.Disposable;
import io.vavr.control.Either;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
@Log4j2
@Component
public class HotVsColdObservables extends HotVsColdReactiveApis {

    // NOTE: This is a simulation of a 3-Subscribers from 1-Observable
    public static void main(String[] args) {
        log.info(
                String.format("Starting %s Observables from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));

        HotVsColdObservables observables = new HotVsColdObservables();

        Observable<CompletableFuture<Either<String,Integer>>> observable =
                observables.createObservable(DEFAULT_COLD_OBSERVABLE, DEFAULT_THRESHOLD);

        observables.runObservableForSubscriberNbr(SUBSCRIBER_NBR_1, observable);

        setTimeout.accept(2000); // delay 2 seconds then start subscriber # 2
        observables.runObservableForSubscriberNbr(SUBSCRIBER_NBR_2, observable);

        setTimeout.accept(2000); // delay 2 seconds then start subscriber # 3
        observables.runObservableForSubscriberNbr(SUBSCRIBER_NBR_3, observable);

        boolean anySubscribersStillListening;
        do {
            Collection<Disposable> disposables =
                    observables.mapOfDisposable
                            .values()
                            .stream()
                            .filter(Objects::nonNull)
                            .filter(disposable -> ! disposable.isDisposed())
                            .collect(Collectors.toList());
            anySubscribersStillListening = ! disposables.isEmpty();
            setTimeout.accept(1000);
        } while (anySubscribersStillListening);

        log.info(
                String.format("DONE with %s Observables from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));
    }

    // NOTE: This is for a simulation of a 3-Subscribers from 1-Observable, from main()
    private void runObservableForSubscriberNbr(
            final Integer subscriberNbr
            , final Observable<CompletableFuture<Either<String,Integer>>> observable
    ) {
        observable
                .doOnSubscribe(disposable -> this.mapOfDisposable.put(subscriberNbr, disposable))
                .map(promise -> doubleIt.apply(subscriberNbr).apply(promise))
                .map(promise -> resetIt.apply(subscriberNbr).apply(promise))
                .subscribe(
                        promise -> onNext.apply(subscriberNbr).accept(promise)
                        , error -> onError.apply(subscriberNbr).accept(error)
                        , () -> onComplete.accept(subscriberNbr)
                );
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
                Observable.<Integer>create(observer -> nextPrime(START_PRIME_AT_1, threshold, observer))
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

        return createObservable(isHotObservable, threshold)
                .map(doubleThePrime::apply)
                .map(resetThePrime::apply)
                .doOnNext(promise -> {
                    promise.thenAccept(either -> {
                        log.info(
                                String.format("Observable from %s - \t%s\t%s\t - doOnNext()"
                                        , Thread.currentThread().getName()
                                        , either.isRight() ? String.format("Value : %s", either.get().toString()) : ""
                                        , either.isLeft() ? String.format("\tError : %s", either.getLeft()) : ""
                                ));
                    });
                })
                .doOnError(error ->
                        log.error(
                                String.format("Observable caught an error from %s - \t%s\t - doOnError()"
                                        , Thread.currentThread().getName()
                                        , error.getMessage()
                                ))
                )
                .onErrorReturn(error -> {
                    log.error(
                            String.format("Observable caught an error from %s - \t%s\t - onErrorReturn()"
                                    , Thread.currentThread().getName()
                                    , error.getMessage()
                            ));
                    return CompletableFuture.supplyAsync(() -> {
                        setTimeout.accept(100);
                        return Either.left(error.getMessage());
                    });
                })
                .onErrorResumeNext(error -> {
                    log.error(
                            String.format("Observable caught an error from %s - \t%s\t - onErrorResumeNext()"
                                    , Thread.currentThread().getName()
                                    , error.getMessage()
                            ));
                    return Observable.just(
                            CompletableFuture.supplyAsync(() -> {
                                setTimeout.accept(100);
                                return Either.left(error.getMessage());
                            })
                    );
                })
                ;
    }
}