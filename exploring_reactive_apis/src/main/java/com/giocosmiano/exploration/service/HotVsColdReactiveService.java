package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.HotVsColdEither;
import com.giocosmiano.exploration.reactiveApis.HotVsColdObservables;
import com.giocosmiano.exploration.reactiveApis.HotVsColdReactorFlux;
import com.sun.xml.internal.ws.util.CompletedFuture;
import io.reactivex.Observable;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
@Service
public class HotVsColdReactiveService {

    protected static final Logger log = LoggerFactory.getLogger(HotVsColdReactiveService.class);

    private HotVsColdObservables hotVsColdObservables;
    private HotVsColdReactorFlux hotVsColdReactorFlux;

    public HotVsColdReactiveService(
            HotVsColdObservables hotVsColdObservables
            , HotVsColdReactorFlux hotVsColdReactorFlux
    ) {
        this.hotVsColdObservables = hotVsColdObservables;
        this.hotVsColdReactorFlux = hotVsColdReactorFlux;
    }

    public Observable<HotVsColdEither> getStreamObservablePrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {
        // TODO: This smells bad. Need to remove the layer CompletableFuture between Flux and its inner value
        return hotVsColdObservables.runObservable(isHotObservable, threshold)
                .map(promise -> {
                    return promise.thenApply(either -> {
                        HotVsColdEither hotVsColdEither = new HotVsColdEither();
                        if (either.isRight()) {
                            hotVsColdEither.setRightValue(either.get());
                        } else {
                            hotVsColdEither.setLeftValue(either.getLeft());
                        }
                        return hotVsColdEither;
                    }).get();  // this is blocking
                })
                ;
    }

    public Single<List<HotVsColdEither>> getObservablePrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {
        AtomicReference<List<HotVsColdEither>> atomicReference = new AtomicReference<>();
        atomicReference.set(new ArrayList<>());

        return hotVsColdObservables.runObservable(isHotObservable, threshold)
                .map(promise -> {
                    promise.thenAccept(either -> {
                        HotVsColdEither hotVsColdEither = new HotVsColdEither();
                        if (either.isRight()) {
                            hotVsColdEither.setRightValue(either.get());
                        } else {
                            hotVsColdEither.setLeftValue(either.getLeft());
                        }
                        atomicReference.get().add(hotVsColdEither);
                    });
                    return atomicReference.get();
                })
                .lastElement()
                .toSingle()
                ;
    }

    public Flux<HotVsColdEither> getStreamFluxPrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {

        // TODO: This smells really, really bad. Need to remove the layer CompletableFuture between Flux and its inner value
        return hotVsColdReactorFlux.runReactorFux(isHotObservable, threshold)
                .map(promise -> {
                    HotVsColdEither newHotVsColdEither = null;
                    try {
                        newHotVsColdEither =
                                promise.thenApply(either -> {
                                    HotVsColdEither hotVsColdEither = new HotVsColdEither();
                                    if (either.isRight()) {
                                        hotVsColdEither.setRightValue(either.get());
                                    } else {
                                        hotVsColdEither.setLeftValue(either.getLeft());
                                    }
                                    return hotVsColdEither;
                                }).get(); // this is blocking
                    } catch (Exception e) { }
                    return newHotVsColdEither;
                })
                ;
    }

    public Mono<List<HotVsColdEither>> getFluxPrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {
        AtomicReference<List<HotVsColdEither>> atomicReference = new AtomicReference<>();
        atomicReference.set(new ArrayList<>());

        return hotVsColdReactorFlux.runReactorFux(isHotObservable, threshold)
                .map(promise -> {
                    promise.thenAccept(either -> {
                        HotVsColdEither hotVsColdEither = new HotVsColdEither();
                        if (either.isRight()) {
                            hotVsColdEither.setRightValue(either.get());
                        } else {
                            hotVsColdEither.setLeftValue(either.getLeft());
                        }
                        atomicReference.get().add(hotVsColdEither);
                    });
                    return atomicReference.get();
                })
                .last()
                ;
    }
}