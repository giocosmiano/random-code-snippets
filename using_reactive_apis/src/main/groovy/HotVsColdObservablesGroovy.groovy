import io.reactivex.Observable
import io.reactivex.ObservableEmitter
import io.reactivex.disposables.Disposable
import io.reactivex.functions.Consumer

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import java.util.function.Function

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
        Integer iNbr = number
        iNbr++
        while (! isPrime(iNbr)) iNbr++
        iNbr
    }

    def doOnNext = { Boolean isHotObservable, Integer subscriberNbr, Integer data ->
        println(
                String.format("%s Observables from %s - \t%s\t%s\t%s\t - doOnNext()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", data) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", data) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", data) : ""
                ))

        if (subscriberNbr == 1) {
            subscriber1.set(data)

        } else if (subscriberNbr == 2) {
            subscriber2.set(data)

        } else if (subscriberNbr == 3) {
            subscriber3.set(data)
        }
    }

    def doOnError = { Boolean isHotObservable, Integer subscriberNbr, Throwable error ->
        println(
                String.format("%s Observables from %s - \t%s\t%s\t%s\t - doOnError()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", error.getMessage()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", error.getMessage()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", error.getMessage()) : ""
                ))
    }

    def doOnComplete = { Boolean isHotObservable, Integer subscriberNbr ->
        System.out.println(
                String.format("%s Observables from %s - \t%s\t%s\t%s\t - doOnComplete()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", subscriber1.get()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", subscriber2.get()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", subscriber3.get()) : ""
                ))

        if (subscriberNbr == 1) {
            disposable1.dispose()

        } else if (subscriberNbr == 2) {
            disposable2.dispose()

        } else if (subscriberNbr == 3) {
            disposable3.dispose()
        }
    }


    def nextPrime = { Integer number, ObservableEmitter<Integer> observer ->
        final Integer prime = getNextPrime(number)

        if (prime >= 500) {
            observer.onComplete()

            // https://github.com/ReactiveX/RxJava/wiki/Error-Handling
            // https://github.com/ReactiveX/RxJava/wiki/What's-different-in-2.0#error-handling
            // un-comment to simulate an `onError` and `tryOnError` that will halt the entire stream of data
//        } else if (prime >= 200) {
//            observer.onError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime))
//            observer.tryOnError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime))
        }

        if (! observer.isDisposed()) {
            observer.onNext(prime)
        }

        CompletableFuture.supplyAsync({ ->
            sleep(100)
            nextPrime(prime, observer)
            prime
        })
    }

    def runObservable = { boolean isHotObservable ->
        println(
                String.format("Starting %s Observables from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ))

        // NOTE: Using closure since I'm using groovy 2.5.8 and only groovy 2.6+ supports lambdas
        // https://stackoverflow.com/questions/23906748/groovy-compiler-does-not-accept-java-8-lambdas

        Observable<Integer> observable = Observable.<Integer>create({ observer -> nextPrime(1, observer) } )
                .switchMap({prime ->
                    Observable<Integer> disposableStream$ = Observable.just(prime)
                    return disposableStream$
                            .map({data ->
                                if (data >= 100 && data <= 200) {
                                    throw new RuntimeException(String.format("Simulating an error skipping prime=%s, in-between 100 and 200, while continue streaming the rest", prime))
                                }
                                return data
                            })
                            .onErrorReturn({error ->
                                System.out.println(String.format("Caught an error=%s", error.getMessage()))
                                return 0
                            })
                })

        if (isHotObservable) observable = observable.share()

        Function<Integer, Consumer<Integer>> onNext =
                { subscriberNbr -> { data -> doOnNext(isHotObservable, subscriberNbr, data) } }

        Function<Integer, Consumer<Throwable>> onError =
                { subscriberNbr -> { error -> doOnError(isHotObservable, subscriberNbr, error) } }

        Consumer<Integer> onComplete =
                { subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr) }

        Function<Integer, Consumer<Disposable>> onSubscribe =
                { subscriberNbr -> { disposable ->
                    if (subscriberNbr == 1) {
                        disposable1 = disposable

                    } else if (subscriberNbr == 2) {
                        disposable2 = disposable

                    } else if (subscriberNbr == 3) {
                        disposable3 = disposable
                    }
                } }

        observable
                .doOnSubscribe({disposable -> onSubscribe.apply(1).accept(disposable) } )
                .subscribe(
                        {data -> onNext.apply(1).accept(data) }
                        ,{error -> onError.apply(1).accept(error) }
                        ,{ -> onComplete.accept(1) }
                )

        sleep(2000)
        observable
                .doOnSubscribe({disposable -> onSubscribe.apply(2).accept(disposable) } )
                .subscribe(
                        {data -> onNext.apply(2).accept(data) }
                        ,{error -> onError.apply(2).accept(error) }
                        ,{ -> onComplete.accept(2) }
                )

        sleep(2000)
        observable
                .doOnSubscribe({disposable -> onSubscribe.apply(3).accept(disposable) } )
                .subscribe(
                        {data -> onNext.apply(3).accept(data) }
                        ,{error -> onError.apply(3).accept(error) }
                        ,{ -> onComplete.accept(3) }
                )

        sleep(10000)
        println(
                String.format("DONE with %s Observables from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ))
    }
}