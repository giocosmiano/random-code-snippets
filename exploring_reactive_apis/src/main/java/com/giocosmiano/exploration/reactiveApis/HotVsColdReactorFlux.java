package com.giocosmiano.exploration.reactiveApis;

import io.vavr.control.Either;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;

import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
@Component
public class HotVsColdReactorFlux extends HotVsColdReactiveApis {

    private static final Logger log = LoggerFactory.getLogger(HotVsColdReactorFlux.class);

    public static void main(String[] args) {
        log.info(
                String.format("Starting %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));

        HotVsColdReactorFlux flux = new HotVsColdReactorFlux();
        flux.runObservable(DEFAULT_COLD_OBSERVABLE, DEFAULT_THRESHOLD);

        boolean anySubscribersStillListening;
        do {
            anySubscribersStillListening =
                    (
                            (flux.disposable1 != null && ! flux.disposable1.isDisposed())
                                    || (flux.disposable2 != null && ! flux.disposable2.isDisposed())
                                    || (flux.disposable3 != null && ! flux.disposable3.isDisposed())
                    );
            setTimeout.accept(1000);
        } while (anySubscribersStillListening);

        log.info(
                String.format("DONE with %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));
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
                Flux.<Integer>create(observer -> nextPrime(1, threshold, observer))
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
    public Flux<CompletableFuture<Either<String,Integer>>> runObservable(boolean isHotObservable, final Integer threshold) {
        Flux<CompletableFuture<Either<String, Integer>>> flux = createFlux(isHotObservable, threshold);

        Consumer<Integer> onComplete =
                subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr);

        return flux
                .map(promise -> doubleIt.apply(1).apply(promise))
                .map(promise -> resetIt.apply(1).apply(promise))
                ;
    }
}