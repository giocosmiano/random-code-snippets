package com.giocosmiano.exploration.reactiveApis;

import io.vavr.control.Either;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Component;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;

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
public class HotVsColdReactorFlux extends HotVsColdReactiveApis {

    // NOTE: This is a simulation of a 3-Subscribers from 1-Reactor Flux
    public static void main(String[] args) {
        log.info(
                String.format("Starting %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));

        HotVsColdReactorFlux reactorFlux = new HotVsColdReactorFlux();

        Flux<CompletableFuture<Either<String, Integer>>> flux =
                reactorFlux.createFlux(DEFAULT_COLD_OBSERVABLE, DEFAULT_THRESHOLD);

        reactorFlux.runFluxForSubscriberNbr(SUBSCRIBER_NBR_1, flux);

        setTimeout.accept(2000); // delay 2 seconds then start subscriber # 2
        reactorFlux.runFluxForSubscriberNbr(SUBSCRIBER_NBR_2, flux);

        setTimeout.accept(2000); // delay 2 seconds then start subscriber # 3
        reactorFlux.runFluxForSubscriberNbr(SUBSCRIBER_NBR_3, flux);

        boolean anySubscribersStillListening;
        do {
            Collection<reactor.core.Disposable> disposables =
                    reactorFlux.mapOfDisposableFlux
                            .values()
                            .stream()
                            .filter(Objects::nonNull)
                            .filter(disposable -> ! disposable.isDisposed())
                            .collect(Collectors.toList());
            anySubscribersStillListening =  ! disposables.isEmpty();
            setTimeout.accept(1000);
        } while (anySubscribersStillListening);

        log.info(
                String.format("DONE with %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));
    }

    // NOTE: This is for a simulation of a 3-Subscribers from 1-Reactor Flux, from main()
    private void runFluxForSubscriberNbr(
            final Integer subscriberNbr
            , final Flux<CompletableFuture<Either<String, Integer>>> flux
    ) {
        Disposable disposable = flux
                .map(promise -> doubleIt.apply(subscriberNbr).apply(promise))
                .map(promise -> resetIt.apply(subscriberNbr).apply(promise))
                .subscribe(
                        promise -> onNext.apply(subscriberNbr).accept(promise)
                        , error -> onError.apply(subscriberNbr).accept(error)
                        , () -> onComplete.accept(subscriberNbr)
                );
        this.mapOfDisposableFlux.put(subscriberNbr, disposable);
    }

    private void nextPrime(final Integer number, final Integer threshold, final FluxSink<Integer> observer) {
        final Integer prime = getNextPrime.apply(number);

        // Emit a completion when threshold is reached
        if (prime >= threshold) {
            CompletableFuture.runAsync(() -> {
                setTimeout.accept(500);
                observer.complete();
            });

            // https://github.com/ReactiveX/RxJava/wiki/Error-Handling
            // https://github.com/ReactiveX/RxJava/wiki/What's-different-in-2.0#error-handling
            // un-comment to simulate an `onError` that will halt the entire stream of data
//        } else if (prime >= 200) {
//            observer.error(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime));

            // emit the next data
        } else {
            observer.next(prime);

            CompletableFuture.supplyAsync(() -> {
                setTimeout.accept(100);
                nextPrime(prime, threshold, observer);
                return prime;
            });
        }
    }

    private Flux<CompletableFuture<Either<String,Integer>>> createFlux(boolean isHotObservable, final Integer threshold) {
        Flux<CompletableFuture<Either<String, Integer>>> flux =
                Flux.<Integer>create(observer -> nextPrime(START_PRIME_AT_1, threshold, observer))
                        .switchMap(prime -> {

                            CompletableFuture<Either<String, Integer>> cf =
                                    CompletableFuture.supplyAsync(() -> {
                                        setTimeout.accept(100);
                                        return Either.right(prime);
                                    });
                            Flux<CompletableFuture<Either<String, Integer>>> disposableStream$ = Flux.just(cf);
                            return disposableStream$;
                        });

        if (isHotObservable) flux = flux.share();

        return flux;
    }

    // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
    // http://localhost:8080/getEmployeeDetails/123
    // CompletableFuture.supplyAsync(() -> getEmployee(empId))
    //         .thenApply(emp -> getEmployeeDept(empId))
    //         .thenApply(emp -> getEmployeePay(empId));
    public Flux<CompletableFuture<Either<String,Integer>>> runReactorFux(boolean isHotObservable, final Integer threshold) {

        return createFlux(isHotObservable, threshold)
                .map(promise -> doubleThePrime.apply(promise))
                .map(promise -> resetThePrime.apply(promise))
                .doOnNext(promise -> {
                    promise.thenAccept(either -> {
                        log.info(
                                String.format("Reactor Flux from %s - \t%s\t%s\t - doOnNext()"
                                        , Thread.currentThread().getName()
                                        , either.isRight() ? String.format("Value : %s", either.get().toString()) : ""
                                        , either.isLeft() ? String.format("\tError : %s", either.getLeft()) : ""
                                ));
                    });
                })
                .doOnError(error ->
                        log.error(
                                String.format("Reactor Flux caught an error from %s - \t%s\t - doOnError()"
                                        , Thread.currentThread().getName()
                                        , error.getMessage()
                                ))
                )
                .onErrorResume(error -> {
                    log.error(
                            String.format("Reactor Flux caught an error from %s - \t%s\t - onErrorResume()"
                                    , Thread.currentThread().getName()
                                    , error.getMessage()
                            ));
                    return Flux.just(CompletableFuture.supplyAsync(() -> {
                        setTimeout.accept(100);
                        return Either.left(error.getMessage());
                    }));
                })
                ;
    }
}