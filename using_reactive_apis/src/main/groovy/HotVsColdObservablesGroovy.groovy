import io.reactivex.Observable
import io.reactivex.ObservableEmitter
import io.reactivex.disposables.Disposable

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger

class HotVsColdObservablesGroovy {
    private AtomicInteger subscriber1 = new AtomicInteger()
    private AtomicInteger subscriber2 = new AtomicInteger()
    private AtomicInteger subscriber3 = new AtomicInteger()

    private Disposable disposable1 = null
    private Disposable disposable2 = null
    private Disposable disposable3 = null

    static void main(String[] args) {
        HotVsColdObservablesGroovy observables = new HotVsColdObservablesGroovy()
        observables.runObservable(false)
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

    def doOnNext = { Boolean isHotObservable, Integer subscriberNbr, Integer data ->
        if (subscriberNbr == 1) {
            subscriber1.set(data)

        } else if (subscriberNbr == 2) {
            subscriber2.set(data)

        } else if (subscriberNbr == 3) {
            subscriber3.set(data)
        }

        println(
                "${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()} - \t"
                        + (subscriberNbr == 1 ? "Subscriber 1: ${subscriber1.get()}" : "")
                        + (subscriberNbr == 2 ? "\tSubscriber 2: ${subscriber2.get()}" : "")
                        + (subscriberNbr == 3 ? "\t\tSubscriber 3: ${subscriber3.get()}" : "")
                        + "\t - from doOnNext()"
        )
    }

    def doOnError = { Boolean isHotObservable, Integer subscriberNbr, Throwable error ->
        println(
                "${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()} - \t"
                        + (subscriberNbr == 1 ? "Subscriber 1: ${error.getMessage()}" : "")
                        + (subscriberNbr == 2 ? "\tSubscriber 2: ${error.getMessage()}" : "")
                        + (subscriberNbr == 3 ? "\t\tSubscriber 3: ${error.getMessage()}" : "")
                        + "\t - from doOnError()"
        )
    }

    def doOnComplete = { Boolean isHotObservable, Integer subscriberNbr ->
        if (subscriberNbr == 1) {
            disposable1.dispose()

        } else if (subscriberNbr == 2) {
            disposable2.dispose()

        } else if (subscriberNbr == 3) {
            disposable3.dispose()
        }

        println(
                "${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()} - \t"
                        + (subscriberNbr == 1 ? "Subscriber 1: ${subscriber1.get()}" : "")
                        + (subscriberNbr == 2 ? "\tSubscriber 2: ${subscriber2.get()}" : "")
                        + (subscriberNbr == 3 ? "\t\tSubscriber 3: ${subscriber3.get()}" : "")
                        + "\t - from doOnComplete()"
        )
    }


    def nextPrime = { Integer number, ObservableEmitter<Integer> observer ->
        final Integer prime = getNextPrime(number)

        // Emit a completion when threshold is reached
        if (prime >= 500) {
            CompletableFuture.runAsync({
                sleep(100)
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
                nextPrime(prime, observer)
                prime
            })
        }
    }


    def runObservable = { boolean isHotObservable ->
        println("Starting ${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()}")

        // NOTE: Using closure since I'm using groovy 2.5.8 and only groovy 2.6+ supports lambdas
        // https://stackoverflow.com/questions/23906748/groovy-compiler-does-not-accept-java-8-lambdas

        // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
        // http://localhost:8080/getEmployeeDetails/123
        // CompletableFuture.supplyAsync(() -> getEmployee(empId))
        //         .thenApply(emp -> getEmployeeDept(empId))
        //         .thenApply(emp -> getEmployeePay(empId))
        Observable<CompletableFuture<Integer>> observable =
                Observable.<Integer>create({ ObservableEmitter<Integer> observer -> nextPrime(1, observer) })
                        .<CompletableFuture<Integer>>switchMap({ Integer prime ->

                            // Simulating a non-blocking IO e.g. ReST call, but for now just doubling the prime value
                            CompletableFuture<Integer> cf =
                                    CompletableFuture.supplyAsync({
                                        sleep(100)
                                        prime * 2 // double the value
                                    })
                            Observable<CompletableFuture<Integer>> disposableStream$ = Observable.just(cf)

                            disposableStream$
                                    .map({ CompletableFuture<Integer> promise ->

                                        // using promise.get() as promise.thenApply() will throw an exception on data >= 100 && data <= 200 thus causing the stream to shut off
                                        Integer data = promise.get()
                                        if (data >= 100 && data <= 200) {
                                            throw new RuntimeException("Simulating an error skipping prime=$data, in-between 100 and 200, while continue streaming the rest")
                                        }

                                        // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just setting it back to original prime
                                        CompletableFuture.supplyAsync({
                                            sleep(100)
                                            data / 2 // set it back to original `prime` after doubling the value
                                        })

                                        // this promise.thenApply() closure will throw an exception thus shutting off the stream of data
//                                        promise.thenApply({ Integer data ->
//                                            if (data >= 100 && data <= 200) {
//                                                throw new RuntimeException("Simulating an error skipping prime=$data, in-between 100 and 200, while continue streaming the rest")
//                                            }
//
//                                            // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just setting it back to original prime
//                                            sleep(100)
//                                            data / 2 // set it back to original `prime` after doubling the value
//                                        })
//                                        .exceptionally({ Throwable error -> throw new RuntimeException(error.getMessage()) })
                                    })
                                    .onErrorReturn({ Throwable error ->
                                        println("Caught an error=${error.getMessage()}")
                                        CompletableFuture.supplyAsync({
                                            sleep(100)
                                            0
                                        })
                                    })
                        })

        if (isHotObservable) observable = observable.share()

        // using promise.get() as promise.thenAccept() doesn't seem to work on closure
        def onNext =
                { Integer subscriberNbr, CompletableFuture<Integer> promise ->
                        doOnNext(isHotObservable, subscriberNbr, (Integer)promise.get()) }
//        def onNext =
//                { Integer subscriberNbr, CompletableFuture<Integer> promise ->
//                    promise.thenAccept({ Integer data ->
//                        doOnNext(isHotObservable, subscriberNbr, data) } ) }

        def onError =
                { Integer subscriberNbr, Throwable error -> doOnError(isHotObservable, subscriberNbr, error) }

        def onComplete =
                { Integer subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr) }

        def onSubscribe =
                { Integer subscriberNbr, Disposable disposable ->
                    if (subscriberNbr == 1) {
                        disposable1 = disposable

                    } else if (subscriberNbr == 2) {
                        disposable2 = disposable

                    } else if (subscriberNbr == 3) {
                        disposable3 = disposable
                    }
                }

        observable
                .doOnSubscribe({ Disposable disposable -> onSubscribe(1, disposable) } )
                .subscribe(
                        { CompletableFuture<Integer> promise -> onNext(1, promise) }
                        ,{ Throwable error -> onError(1, error) }
                        ,{ onComplete(1) }
                )

        sleep(2000)
        observable
                .doOnSubscribe({ Disposable disposable -> onSubscribe(2, disposable) } )
                .subscribe(
                        { CompletableFuture<Integer> promise -> onNext(2, promise) }
                        ,{ Throwable error -> onError(2, error) }
                        ,{ onComplete(2) }
                )

        sleep(2000)
        observable
                .doOnSubscribe({ Disposable disposable -> onSubscribe(3, disposable) } )
                .subscribe(
                        { CompletableFuture<Integer> promise -> onNext(3, promise) }
                        ,{ Throwable error -> onError(3, error) }
                        ,{ onComplete(3) }
                )

        def anySubscribersStillListening = {
            sleep(1000)
            (
                    (disposable1 != null && ! disposable1.isDisposed())
                            || (disposable2 != null && ! disposable2.isDisposed())
                            || (disposable3 != null && ! disposable3.isDisposed())
            )
        }
        while (anySubscribersStillListening.call()) continue

        println("DONE with ${isHotObservable ? 'Hot' : 'Cold'} Observables from ${Thread.currentThread().getName()}")
    }
}