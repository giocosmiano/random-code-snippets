package com.giocosmiano.exploration.reactiveApis;

import io.reactivex.disposables.Disposable;
import io.vavr.control.Either;
import lombok.extern.log4j.Log4j2;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.setTimeout;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
@Log4j2
public abstract class HotVsColdReactiveApis {

    protected final AtomicInteger subscriber1 = new AtomicInteger();
    protected final AtomicInteger subscriber2 = new AtomicInteger();
    protected final AtomicInteger subscriber3 = new AtomicInteger();

    protected Map<Integer, Disposable> mapOfDisposable = new HashMap<>();
    protected Map<Integer, reactor.core.Disposable> mapOfDisposableFlux = new HashMap<>();

    protected static boolean isHotObservable = false;

    // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a Consumer applying a timeout and doubling the value
    protected Function<CompletableFuture<Either<String,Integer>>, CompletableFuture<Either<String,Integer>>> doubleThePrime =
            promise -> promise.thenApply(either -> {

                setTimeout.accept(100);
                Integer data = either.get();
                Integer newValue = data * 2;

                // Simulating an error using Either.left()
                if (newValue >= 100 && newValue <= 200) {
                    String error = String.format("Simulating an error skipping double value of prime in-between 100 and 200, where prime=%s and double=%s", data, newValue);

                    // TODO: Need to find out why Observable.onErrorReturn(), Observable.onErrorResumeNext(), Flux.onErrorResume() is NOT being
                    // executed when throwing a RuntimeException exception within CompletableFuture
                    // Somehow this is causing Observable/Flux to shut streaming data, rather than continuing
                    // to stream the rest of data, which includes errors as part of it (Either.left)
//                     throw new RuntimeException(error);

                    // comment this to simulate the error above
                    return Either.left(error);
                }

                return Either.right(newValue);
            })
            ;

    protected Function<Integer, Function<CompletableFuture<Either<String,Integer>>, CompletableFuture<Either<String,Integer>>>> doubleIt =
            subscriberNbr -> promise -> doubleThePrime.apply(promise);

    // Simulating a non-blocking IO e.g. ReST call, but for now just a Consumer applying a timeout and setting it back to original prime
    protected Function<CompletableFuture<Either<String,Integer>>, CompletableFuture<Either<String,Integer>>> resetThePrime =
            promise -> promise.thenApply(either -> {
                setTimeout.accept(100);

                if (either.isRight()) {
                    Integer data = either.get();
                    Integer newValue = data / 2;
                    return Either.right(newValue);
                }

                return either;
            });

    protected Function<Integer, Function<CompletableFuture<Either<String,Integer>>, CompletableFuture<Either<String,Integer>>>> resetIt =
            subscriberNbr -> promise -> resetThePrime.apply(promise);

    protected Function<Integer, Consumer<CompletableFuture<Either<String,Integer>>>> onNext =
            subscriberNbr -> promise -> promise.thenAccept(either -> doOnNext(isHotObservable, subscriberNbr, either));

    protected Function<Integer, Consumer<Throwable>> onError =
            subscriberNbr -> error -> doOnError(isHotObservable, subscriberNbr, error);

    protected Consumer<Integer> onComplete =
            subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr);

    public void doOnNext(boolean isHotObservable, final Integer subscriberNbr, final Either<String,Integer> either) {
        String message;

        if (either.isRight()) {
            Integer data = either.get();
            message = String.valueOf(data);

            if (subscriberNbr == 1) {
                subscriber1.set(data);

            } else if (subscriberNbr == 2) {
                subscriber2.set(data);

            } else if (subscriberNbr == 3) {
                subscriber3.set(data);
            }

        } else {
            message = either.getLeft();
        }

        log.info(
                String.format("%s from %s - \t%s\t%s\t%s\t - doOnNext()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", message) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", message) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", message) : ""
                ));
    }

    public void doOnError(boolean isHotObservable, final Integer subscriberNbr, final Throwable error) {
        log.info(
                String.format("%s from %s - \t%s\t%s\t%s\t - doOnError()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", error.getMessage()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", error.getMessage()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", error.getMessage()) : ""
                ));
    }

    public void doOnComplete(boolean isHotObservable, final Integer subscriberNbr) {

        reactor.core.Disposable disposableFlux;
        Disposable disposable = mapOfDisposable.get(subscriberNbr);

        if (disposable != null) {
            disposable.dispose();
        } else {

            disposableFlux = mapOfDisposableFlux.get(subscriberNbr);
            if (disposableFlux != null) {
                disposableFlux.dispose();
            }
        }

        log.info(
                String.format("%s from %s - \t%s\t%s\t%s\t - doOnComplete()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", subscriber1.get()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", subscriber2.get()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", subscriber3.get()) : ""
                ));
    }
}