package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.HotVsColdEither;
import com.giocosmiano.exploration.reactiveApis.HotVsColdObservables;
import com.giocosmiano.exploration.reactiveApis.HotVsColdReactorFlux;
import io.reactivex.Observable;
import io.reactivex.Single;
import io.reactivex.schedulers.Schedulers;
import lombok.extern.log4j.Log4j2;
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
@Log4j2
@Service
public class HotVsColdReactiveService {

    private final HotVsColdObservables hotVsColdObservables;
    private final HotVsColdReactorFlux hotVsColdReactorFlux;

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
        return hotVsColdObservables
                .runObservable(isHotObservable, threshold)
                .flatMap(promise -> {
                    return Observable.fromFuture(
                            promise.thenApply(either -> {
                                HotVsColdEither hotVsColdEither = new HotVsColdEither();
                                if (either.isRight()) {
                                    hotVsColdEither.setRightValue(either.get());
                                } else {
                                    hotVsColdEither.setLeftValue(either.getLeft());
                                }
                                return hotVsColdEither;
                            })
                    ).subscribeOn(Schedulers.computation()) // running on different thread
                    ;
                })
/*
                // TODO: This smells bad. Use the above flatMap() to remove the layer CompletableFuture in-between Observable and its inner value
                .map(promise -> {
                    return promise.thenApply(either -> {
                        HotVsColdEither hotVsColdEither = new HotVsColdEither();
                        if (either.isRight()) {
                            hotVsColdEither.setRightValue(either.get());
                        } else {
                            hotVsColdEither.setLeftValue(either.getLeft());
                        }
                        return hotVsColdEither;
                    }).get();  // this is blocking call
                })
*/
                ;
    }

    // Another implementation of getObservablePrimesOLD() below getting around Http response 503
    public Single<List<HotVsColdEither>> getObservablePrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {
        AtomicReference<List<HotVsColdEither>> atomicReference = new AtomicReference<>();
        atomicReference.set(new ArrayList<>());

        return hotVsColdObservables
                .runObservable(isHotObservable, threshold)
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

    // Another implementation of getObservablePrimes() above that's causing Http 503 due to CompletableFuture within Observable
    // org.springframework.web.context.request.async.AsyncRequestTimeoutException
    // https://stackoverflow.com/questions/39856198/recurring-asyncrequesttimeoutexception-in-spring-boot-admin-log
    // https://stackoverflow.com/questions/53650303/spring-boot-timeout-when-using-futures/53652640
    @Deprecated
    public Observable<HotVsColdEither> getObservablePrimesOLD(
            boolean isHotObservable
            , final Integer threshold
    ) {
        return hotVsColdObservables
                .runObservable(isHotObservable, threshold)
                .flatMap(promise -> {
                    return Observable.fromFuture(
                            promise.thenApply(either -> {
                                HotVsColdEither hotVsColdEither = new HotVsColdEither();
                                if (either.isRight()) {
                                    hotVsColdEither.setRightValue(either.get());
                                } else {
                                    hotVsColdEither.setLeftValue(either.getLeft());
                                }
                                return hotVsColdEither;
                            })
                    ).subscribeOn(Schedulers.computation()) // running on different thread
                    ;
                })
                ;
    }

    public Flux<HotVsColdEither> getStreamFluxPrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {
        // Converting Mono to Flux
        // https://stackoverflow.com/questions/47399707/how-do-we-convert-a-monolisttype-to-a-fluxtype
        return hotVsColdReactorFlux
                .runReactorFux(isHotObservable, threshold)
                .flatMap(promise -> {
                    return Mono.<List<HotVsColdEither>>fromFuture(
                            promise.thenApply(either -> {
                                List<HotVsColdEither> listOfEithers = new ArrayList<>();
                                HotVsColdEither hotVsColdEither = new HotVsColdEither();
                                if (either.isRight()) {
                                    hotVsColdEither.setRightValue(either.get());
                                } else {
                                    hotVsColdEither.setLeftValue(either.getLeft());
                                }
                                listOfEithers.add(hotVsColdEither);
                                return listOfEithers;
                            })
                    ).flatMapMany(Flux::fromIterable);
                })
/*
                // TODO: This smells really, really bad. Use the above flatMap() to remove the layer CompletableFuture in-between Flux and its inner value
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
                                }).get(); // this is blocking call
                    } catch (Exception e) { }
                    return newHotVsColdEither;
                })
*/
                ;
    }

    public Mono<List<HotVsColdEither>> getFluxPrimes(
            boolean isHotObservable
            , final Integer threshold
    ) {
        AtomicReference<List<HotVsColdEither>> atomicReference = new AtomicReference<>();
        atomicReference.set(new ArrayList<>());

        return hotVsColdReactorFlux
                .runReactorFux(isHotObservable, threshold)
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