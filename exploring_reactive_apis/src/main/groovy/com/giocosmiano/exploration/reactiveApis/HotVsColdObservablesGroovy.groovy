package com.giocosmiano.exploration.reactiveApis

import com.giocosmiano.exploration.domain.HotVsColdEither
import io.reactivex.Observable
import io.reactivex.ObservableEmitter
import io.reactivex.Single
import io.reactivex.disposables.Disposable
import io.reactivex.schedulers.Schedulers
import io.vavr.control.Either
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.setTimeout

class HotVsColdObservablesGroovy {
    private static START_PRIME_AT_1 = 1
    private static SUBSCRIBER_NBR_1 = 1
    private static SUBSCRIBER_NBR_2 = 2
    private static SUBSCRIBER_NBR_3 = 3
    private static DEFAULT_THRESHOLD = 500
    private static DEFAULT_COLD_OBSERVABLE = false

    private AtomicInteger subscriber1 = new AtomicInteger()
    private AtomicInteger subscriber2 = new AtomicInteger()
    private AtomicInteger subscriber3 = new AtomicInteger()

    private static final Logger log = LoggerFactory.getLogger(HotVsColdObservablesGroovy.class)

    static void main(String[] args) {

        Map<Integer, Disposable> mapOfDisposables = [:]
        def isHotObservable = DEFAULT_COLD_OBSERVABLE
        def threshold = DEFAULT_THRESHOLD

        log.info("Starting ${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()}")

        HotVsColdObservablesGroovy observables = new HotVsColdObservablesGroovy()

        def observable = observables.createObservable(isHotObservable, threshold)

        observables.runObservableForSubscriberNbr(isHotObservable, SUBSCRIBER_NBR_1, mapOfDisposables, observable)

        sleep(2000)
        observables.runObservableForSubscriberNbr(isHotObservable, SUBSCRIBER_NBR_2, mapOfDisposables, observable)

        sleep(2000)
        observables.runObservableForSubscriberNbr(isHotObservable, SUBSCRIBER_NBR_3, mapOfDisposables, observable)

        def anySubscribersStillListening = {
            sleep(1000)
            def activeSubscribers = mapOfDisposables.findAll { entry -> entry.value != null && ! entry.value.isDisposed() }
            ! activeSubscribers.isEmpty()
        }
        while (anySubscribersStillListening.call()) continue

        log.info("DONE with ${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()}")
    }

    def isPrime = { Integer number ->
        for (int i = 2; i < number; i++) {
            if (number % i == 0) return false
        }
        true
    }

    def getNextPrime = { Integer number ->
        Integer iNbr = number + 1
        while (! isPrime(iNbr)) iNbr++
        iNbr
    }

    def doOnNext = { Boolean isHotObservable, Integer subscriberNbr, Either<String,Integer> either ->
        String message

        if (either.isRight()) {
            Integer data = either.get()
            message = String.valueOf(data)

            if (subscriberNbr == 1) {
                subscriber1.set(data)

            } else if (subscriberNbr == 2) {
                subscriber2.set(data)

            } else if (subscriberNbr == 3) {
                subscriber3.set(data)
            }

        } else {
            message = either.getLeft()
        }

        log.info(
                "${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()} - \t"
                        + (subscriberNbr == 1 ? "Subscriber 1: ${message}" : "")
                        + (subscriberNbr == 2 ? "\tSubscriber 2: ${message}" : "")
                        + (subscriberNbr == 3 ? "\t\tSubscriber 3: ${message}" : "")
                        + "\t - from doOnNext()"
        )
    }

    def doOnError = { Boolean isHotObservable, Integer subscriberNbr, Throwable error ->
        log.info(
                "${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()} - \t"
                        + (subscriberNbr == 1 ? "Subscriber 1: ${error.getMessage()}" : "")
                        + (subscriberNbr == 2 ? "\tSubscriber 2: ${error.getMessage()}" : "")
                        + (subscriberNbr == 3 ? "\t\tSubscriber 3: ${error.getMessage()}" : "")
                        + "\t - from doOnError()"
        )
    }

    def doOnComplete = { Boolean isHotObservable, Integer subscriberNbr, Map<Integer, Disposable> mapOfDisposables ->
        def disposable = mapOfDisposables[subscriberNbr]
        if (! disposable) {
            disposable.dispose()
        }

        log.info(
                "${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()} - \t"
                        + (subscriberNbr == 1 ? "Subscriber 1: ${subscriber1.get()}" : "")
                        + (subscriberNbr == 2 ? "\tSubscriber 2: ${subscriber2.get()}" : "")
                        + (subscriberNbr == 3 ? "\t\tSubscriber 3: ${subscriber3.get()}" : "")
                        + "\t - from doOnComplete()"
        )
    }

    def nextPrime = { Integer number, Integer threshold, ObservableEmitter<Integer> observer ->
        final Integer prime = getNextPrime(number)

        // Emit a completion when threshold is reached
        if (prime >= threshold) {
            CompletableFuture.runAsync({
                sleep(500)
                observer.onComplete()
            })

            // https://github.com/ReactiveX/RxJava/wiki/Error-Handling
            // https://github.com/ReactiveX/RxJava/wiki/What's-different-in-2.0#error-handling
            // un-comment to simulate an `onError` and `tryOnError` that will halt the entire stream of data
//        } else if (prime >= 200) {
//            observer.onError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime))
//            observer.tryOnError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime))

            // emit the next data
        } else if (! observer.isDisposed()) {
            observer.onNext(prime)

            CompletableFuture.supplyAsync({
                sleep(100)
                nextPrime(prime, threshold, observer)
                prime
            })
        }
    }

    // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a Consumer applying a timeout and doubling the value
    def doubleThePrime =
            { CompletableFuture<Either<String,Integer>> promise ->
                promise.thenApply({ Either<String,Integer> either ->

                    sleep(100)
                    Integer data = either.get()
                    Integer newValue = data * 2

                    // Simulating an error using Either.left()
                    if (newValue >= 100 && newValue <= 200) {
                        String error = "Simulating an error skipping double value of prime in-between 100 and 200, where prime=${data} and double=${newValue}"
                        Either.left(error)

                    } else {
                        Either.right(newValue)
                    }
                })
            }

    // Simulating a non-blocking IO e.g. ReST call, but for now just a Consumer applying a timeout and setting it back to original prime
    def resetThePrime =
            { CompletableFuture<Either<String,Integer>> promise ->
                promise.thenApply({ Either<String,Integer> either ->
                    sleep(100)

                    if (either.isRight()) {
                        Integer data = either.get()
                        Integer newValue = data / 2
                        return Either.right(newValue)
                    }

                    either
                })
            }

    // NOTE: Using closure since I'm using groovy 2.5.8 and only groovy 2.6+ supports lambdas
    // https://stackoverflow.com/questions/23906748/groovy-compiler-does-not-accept-java-8-lambdas

    // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
    // http://localhost:8080/getEmployeeDetails/123
    // CompletableFuture.supplyAsync(() -> getEmployee(empId))
    //         .thenApply(emp -> getEmployeeDept(empId))
    //         .thenApply(emp -> getEmployeePay(empId))
    def createObservable = { boolean isHotObservable, Integer threshold ->
        Observable<CompletableFuture<Either<String,Integer>>> observable =
                Observable.<Integer>create({ ObservableEmitter<Integer> observer -> nextPrime(START_PRIME_AT_1, threshold, observer) })
                        .<CompletableFuture<Either<String,Integer>>>switchMap({ Integer prime ->

                            // Simulation # 1 - a non-blocking IO using doubleThePrime and resetThePrime functions below
                            CompletableFuture<Either<String,Integer>> cf =
                                    CompletableFuture.supplyAsync({
                                        sleep(100)
                                        Either.right(prime)
                                    })
                            Observable<CompletableFuture<Either<String,Integer>>> disposableStream$ = Observable.just(cf)

                            disposableStream$

/*
                            // NOTE: un-comment if want to try different simulation without using doubleThePrime and resetThePrime functions below
                            // Simulation # 2 - a non-blocking IO e.g. ReST call, but for now just doubling the prime value
                            CompletableFuture<Either<String,Integer>> cf =
                                    CompletableFuture.supplyAsync({
                                        sleep(100)
                                        Either.right(prime * 2) // double the value
                                    })
                            Observable<CompletableFuture<Either<String,Integer>>> disposableStream$ = Observable.just(cf)

                            disposableStream$
                                    .map({ CompletableFuture<Either<String,Integer>> promise ->
                                        promise.thenApply({ Either<String,Integer> either ->

                                            // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a Consumer applying a timeout and setting it back to original prime
                                            sleep(100)
                                            Integer data = either.get()
                                            Integer origPrime = data / 2

                                            // Simulating an error using Either.left()
                                            if (data >= 100 && data <= 200) {
                                                String error = "Simulating an error skipping double value of prime in-between 100 and 200, where prime=${origPrime} and double=${data}"
                                                Either.left(error)

                                            } else {
                                                Either.right(data / 2) // set it back to original `prime` after doubling the value
                                            }
                                        })
                                    })
*/
                        })

        if (isHotObservable) observable = observable.share()

        observable
    }

    def runObservableForSubscriberNbr = {
        boolean isHotObservable, Integer subscriberNbr,
        Map<Integer, Disposable> mapOfDisposables, Observable<CompletableFuture<Either<String,Integer>>> observable ->

            // using promise.get() as promise.thenAccept() doesn't seem to work on closure
            def onNext =
                    { CompletableFuture<Either<String,Integer>> promise ->
                        doOnNext(isHotObservable, subscriberNbr, (Either)promise.get()) }
//            def onNext =
//                    { CompletableFuture<Either<String,Integer>> promise ->
//                        promise.thenAccept( { Integer data ->
//                            doOnNext(isHotObservable, subscriberNbr, data) } ) }

            def onError =
                    { Throwable error -> doOnError(isHotObservable, subscriberNbr, error) }

            def onComplete =
                    { -> doOnComplete(isHotObservable, subscriberNbr, mapOfDisposables) }

            observable
                    .doOnSubscribe({ Disposable disposable -> mapOfDisposables[subscriberNbr] = disposable } )
                    .map({ CompletableFuture<Either<String,Integer>> promise -> doubleThePrime(promise) } )
                    .map({ CompletableFuture<Either<String,Integer>> promise -> resetThePrime(promise) } )
                    .subscribe(
                            { CompletableFuture<Either<String,Integer>> promise -> onNext(promise) }
                            , { Throwable error -> onError(error) }
                            , { onComplete() }
                    )
    }

    Single<List<HotVsColdEither>> runObservable(boolean isHotObservable, Integer threshold) {
        def observable = createObservable(isHotObservable, threshold)

        AtomicReference<List<HotVsColdEither>> atomicReference = new AtomicReference<>()
        atomicReference.set(new ArrayList<>())

        observable
                .map({ CompletableFuture<Either<String,Integer>> promise -> doubleThePrime(promise) } )
                .map({ CompletableFuture<Either<String,Integer>> promise -> resetThePrime(promise) } )
                .doOnNext({ CompletableFuture<Either<String,Integer>> promise ->
                    promise.thenAccept({ Either<String,Integer> either ->
                        log.info(
                                String.format("Observable from %s - \t%s\t%s\t - doOnNext()"
                                        , Thread.currentThread().getName()
                                        , either.isRight() ? String.format("Value : %s", either.get().toString()) : ""
                                        , either.isLeft() ? String.format("\tError : %s", either.getLeft()) : ""
                                ))
                    })
                })
                .map({ CompletableFuture<Either<String,Integer>> promise ->
                    promise.thenAccept({ Either<String,Integer> either ->
                        HotVsColdEither hotVsColdEither = new HotVsColdEither()
                        if (either.isRight()) {
                            hotVsColdEither.setRightValue(either.get())
                        } else {
                            hotVsColdEither.setLeftValue(either.getLeft())
                        }
                        atomicReference.get().add(hotVsColdEither)
                    })
                    atomicReference.get()
                } )
                .lastElement()
                .toSingle()

    }

    Observable<HotVsColdEither> runStreamObservable(boolean isHotObservable, Integer threshold) {
        def observable = createObservable(isHotObservable, threshold)

        observable
                .map({ CompletableFuture<Either<String,Integer>> promise -> doubleThePrime(promise) } )
                .map({ CompletableFuture<Either<String,Integer>> promise -> resetThePrime(promise) } )
                .doOnNext({ CompletableFuture<Either<String,Integer>> promise ->
                    promise.thenAccept({ Either<String,Integer> either ->
                        log.info(
                                String.format("Observable from %s - \t%s\t%s\t - doOnNext()"
                                        , Thread.currentThread().getName()
                                        , either.isRight() ? String.format("Value : %s", either.get().toString()) : ""
                                        , either.isLeft() ? String.format("\tError : %s", either.getLeft()) : ""
                                ))
                    })
                })
                .doOnError({ Throwable error ->
                    log.error(
                            String.format("Observable caught an error from %s - \t%s\t - doOnError()"
                                    , Thread.currentThread().getName()
                                    , error.getMessage()
                            ))
                })
                .onErrorReturn({ Throwable error ->
                    log.error(
                            String.format("Observable caught an error from %s - \t%s\t - onErrorReturn()"
                                    , Thread.currentThread().getName()
                                    , error.getMessage()
                            ))
                    CompletableFuture.supplyAsync({ ->
                        setTimeout.accept(100);
                        Either.left(error.getMessage());
                    })
                })
                .flatMap({ CompletableFuture<Either<String,Integer>> promise ->
                    Observable.fromFuture(
                            promise.thenApply({ Either<String,Integer> either ->
                                HotVsColdEither hotVsColdEither = new HotVsColdEither()
                                if (either.isRight()) {
                                    hotVsColdEither.setRightValue(either.get())
                                } else {
                                    hotVsColdEither.setLeftValue(either.getLeft())
                                }
                                hotVsColdEither
                            })
                    ).subscribeOn(Schedulers.computation()) // running on different thread
                })
/*
        // TODO: This smells bad. Use the above flatMap() to remove the layer CompletableFuture in-between Observable and its inner value
                .map( { CompletableFuture<Either<String,Integer>> promise ->
                    promise.thenApply({ Either<String,Integer> either ->
                        HotVsColdEither hotVsColdEither = new HotVsColdEither()
                        if (either.isRight()) {
                            hotVsColdEither.setRightValue(either.get())
                        } else {
                            hotVsColdEither.setLeftValue(either.getLeft())
                        }
                        hotVsColdEither
                    }).get() // this is blocking call
                } )
*/
    }
}