import io.vavr.control.Either;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;

// Difference between RxJava vs Reactor
// https://www.nurkiewicz.com/2019/02/rxjava-vs-reactor.html
// https://stackoverflow.com/questions/56461260/java-spring-webflux-vs-rxjava
// https://medium.com/wolox/reactor-java-meets-reactive-programming-16105c026fc3
// https://www.javacodegeeks.com/2018/08/frameworks-toolkits-make-java-reactive-rxjava-spring-reactor-akka-vert-x-overview.html
public class HotVsColdReactorFlux {

    private final AtomicInteger subscriber1 = new AtomicInteger();
    private final AtomicInteger subscriber2 = new AtomicInteger();
    private final AtomicInteger subscriber3 = new AtomicInteger();

    private Disposable disposable1 = null;
    private Disposable disposable2 = null;
    private Disposable disposable3 = null;

    private final Function<Integer, Boolean> isPrime = number -> {
        for (int i = 2; i < number; i++) {
            if (number % i == 0) return false;
        }
        return true;
    };

    private final Function<Integer, Integer> getNextPrime = number -> {
        Integer iNbr = number + 1;
        while (! isPrime.apply(iNbr)) iNbr++;
        return iNbr;
    };

    public static void main(String[] args) {
        HotVsColdReactorFlux observables = new HotVsColdReactorFlux();
        observables.runObservable(false);
//        observables.runObservableWithManualErrorHandling(false);
    }

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

        System.out.println(
                String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - doOnNext()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", message) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", message) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", message) : ""
                ));
    }

    public void doOnNext(boolean isHotObservable, final Integer subscriberNbr, final Integer data) {
        if (subscriberNbr == 1) {
            subscriber1.set(data);

        } else if (subscriberNbr == 2) {
            subscriber2.set(data);

        } else if (subscriberNbr == 3) {
            subscriber3.set(data);
        }

        System.out.println(
                String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - from doOnNext()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", subscriber1.get()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", subscriber2.get()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", subscriber3.get()) : ""
                ));
    }

    public void doOnError(boolean isHotObservable, final Integer subscriberNbr, final Throwable error) {
        System.out.println(
                String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - doOnError()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", error.getMessage()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", error.getMessage()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", error.getMessage()) : ""
                ));
    }

    public void doOnComplete(boolean isHotObservable, final Integer subscriberNbr) {
        if (subscriberNbr == 1) {
            disposable1.dispose();

        } else if (subscriberNbr == 2) {
            disposable2.dispose();

        } else if (subscriberNbr == 3) {
            disposable3.dispose();
        }

        System.out.println(
                String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - from doOnComplete()"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                        , subscriberNbr == 1 ? String.format("Subscriber 1: %s", subscriber1.get()) : ""
                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: %s", subscriber2.get()) : ""
                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: %s", subscriber3.get()) : ""
                ));
    }

    public void setTimeout(final Integer millis) {
        try { Thread.sleep(millis); } catch (Exception e) {}
    }

    public void nextPrime(final Integer number, final FluxSink<Integer> observer) {
        final Integer prime = getNextPrime.apply(number);

        // Emit a completion when threshold is reached
        if (prime >= 500) {
            CompletableFuture.runAsync(() -> {
                setTimeout(500);
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
                setTimeout(100);
                nextPrime(prime, observer);
                return prime;
            });
        }
    }

    /*
     * Using Either from `vavr` library to continuously stream data with errors, either with the value on right or error on left
     */
    public void runObservable(boolean isHotObservable) {
        System.out.println(
                String.format("Starting %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));

        // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
        // http://localhost:8080/getEmployeeDetails/123
        // CompletableFuture.supplyAsync(() -> getEmployee(empId))
        //         .thenApply(emp -> getEmployeeDept(empId))
        //         .thenApply(emp -> getEmployeePay(empId));
        Flux<CompletableFuture<Either<String,Integer>>> observable =
                Flux.<Integer>create(observer -> nextPrime(1, observer))
                        .switchMap(prime -> {

                            // Simulation # 1 - a non-blocking IO using doubleIt and resetIt functions below
                            CompletableFuture<Either<String,Integer>> cf =
                                    CompletableFuture.supplyAsync(() -> {
                                        setTimeout(100);
                                        return Either.right(prime);
                                    });
                            Flux<CompletableFuture<Either<String,Integer>>> disposableStream$ = Flux.just(cf);

                            return disposableStream$;

/*
                            // NOTE: un-comment if want to try different simulation without using doubleIt and resetIt functions below
                            // Simulation # 2 - a non-blocking IO e.g. ReST call, but for now just doubling the prime value
                            CompletableFuture<Either<String,Integer>> cf =
                                    CompletableFuture.supplyAsync(() -> {
                                        setTimeout(100);
                                        return Either.right(prime * 2); // double the value
                                    });
                            Flux<CompletableFuture<Either<String,Integer>>> disposableStream$ = Flux.just(cf);

                            return disposableStream$
                                    .map(promise -> {
                                        return promise.thenApply(either -> {

                                            // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a Consumer applying a timeout and setting it back to original prime
                                            setTimeout(100);
                                            Integer data = either.get();

                                            // Simulating an error using Either.left()
                                            if (data >= 100 && data <= 200) {
                                                String error = String.format("Simulating an error skipping prime=%s, in-between 100 and 200, while continue streaming the rest", data);
                                                return Either.left(error);

                                            } else {
                                                return either.map(value -> value / 2);  // set it back to original `prime` after doubling the value
                                            }
                                        });
                                    });
*/
                        })
                ;

        if (isHotObservable) observable = observable.share();

        // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a Consumer applying a timeout and doubling the value
        Function<Integer, Function<CompletableFuture<Either<String,Integer>>, CompletableFuture<Either<String,Integer>>>> doubleIt =
                subscriberNbr -> promise -> promise.thenApply(either -> {

                    setTimeout(100);
                    Integer data = either.get();
                    Integer newValue = data * 2; // double the value
//                    System.out.println(
//                            String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - doubleIt()"
//                                    , isHotObservable ? "Hot" : "Cold"
//                                    , Thread.currentThread().getName()
//                                    , subscriberNbr == 1 ? String.format("Subscriber 1: from %s to %s", data, newValue) : ""
//                                    , subscriberNbr == 2 ? String.format("\tSubscriber 2: from %s to %s", data, newValue) : ""
//                                    , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: from %s to %s", data, newValue) : ""
//                            ));

                    // Simulating an error using Either.left()
                    if (newValue >= 100 && newValue <= 200) {
                        String error = String.format("Simulating an error skipping double value of prime in-between 100 and 200, where prime=%s, double=%s", data, newValue);
                        return Either.left(error);

                    } else {
                        return Either.right(newValue);
                    }
                });

        // Simulating a non-blocking IO e.g. ReST call, but for now just a Consumer applying a timeout and setting it back to original prime
        Function<Integer, Function<CompletableFuture<Either<String,Integer>>, CompletableFuture<Either<String,Integer>>>> resetIt =
                subscriberNbr -> promise -> promise.thenApply(either -> {
                    setTimeout(100);

                    if (either.isRight()) {
                        Integer data = either.get();
                        Integer newValue = data / 2;
//                        System.out.println(
//                                String.format("%s Reactor Flux from %s - \t%s\t%s\t%s\t - resetIt()"
//                                        , isHotObservable ? "Hot" : "Cold"
//                                        , Thread.currentThread().getName()
//                                        , subscriberNbr == 1 ? String.format("Subscriber 1: from %s to %s", data, newValue) : ""
//                                        , subscriberNbr == 2 ? String.format("\tSubscriber 2: from %s to %s", data, newValue) : ""
//                                        , subscriberNbr == 3 ? String.format("\t\tSubscriber 3: from %s to %s", data, newValue) : ""
//                                ));
                        return Either.right(newValue);

                    } else {
                        return either;
                    }
                });

        Function<Integer, Consumer<CompletableFuture<Either<String,Integer>>>> onNext =
                subscriberNbr -> promise -> promise.thenAccept(either -> doOnNext(isHotObservable, subscriberNbr, either));

        Function<Integer, Consumer<Throwable>> onError =
                subscriberNbr -> error -> doOnError(isHotObservable, subscriberNbr, error);

        Consumer<Integer> onComplete =
                subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr);

        disposable1 =
                observable
                        .map(promise -> doubleIt.apply(1).apply(promise))
                        .map(promise -> resetIt.apply(1).apply(promise))
                        .subscribe(
                                promise -> onNext.apply(1).accept(promise)
                                , error -> onError.apply(1).accept(error)
                                , () -> onComplete.accept(1)
                        );

        setTimeout(2000);
        disposable2 =
                observable
                        .map(promise -> doubleIt.apply(2).apply(promise))
                        .map(promise -> resetIt.apply(2).apply(promise))
                        .subscribe(
                                promise -> onNext.apply(2).accept(promise)
                                , error -> onError.apply(2).accept(error)
                                , () -> onComplete.accept(2)
                        );

        setTimeout(2000);
        disposable3 =
                observable
                        .map(promise -> doubleIt.apply(3).apply(promise))
                        .map(promise -> resetIt.apply(3).apply(promise))
                        .subscribe(
                                promise -> onNext.apply(3).accept(promise)
                                , error -> onError.apply(3).accept(error)
                                , () -> onComplete.accept(3)
                        );


        boolean anySubscribersStillListening;
        do {
            anySubscribersStillListening =
                    (
                            (disposable1 != null && ! disposable1.isDisposed())
                                    || (disposable2 != null && ! disposable2.isDisposed())
                                    || (disposable3 != null && ! disposable3.isDisposed())
                    );
            setTimeout(1000);
        } while (anySubscribersStillListening);

        System.out.println(
                String.format("DONE with %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));
    }

    public void runObservableWithManualErrorHandling(boolean isHotObservable) {
        System.out.println(
                String.format("Starting %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));

        // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
        // http://localhost:8080/getEmployeeDetails/123
        // CompletableFuture.supplyAsync(() -> getEmployee(empId))
        //         .thenApply(emp -> getEmployeeDept(empId))
        //         .thenApply(emp -> getEmployeePay(empId));
        Flux<CompletableFuture<Integer>> observable =
                Flux.<Integer>create(observer -> nextPrime(1, observer))
                        .switchMap(prime -> {

                            // Simulating a non-blocking IO e.g. ReST call, but for now just doubling the prime value
                            CompletableFuture<Integer> cf =
                                    CompletableFuture.supplyAsync(() -> {
                                        setTimeout(100);
                                        return prime * 2; // double the value
                                    });
                            Flux<CompletableFuture<Integer>> disposableStream$ = Flux.just(cf);

                            return disposableStream$
                                    .map(promise -> {
                                        return promise.thenApply(data -> {

                                            // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a Consumer applying a timeout and setting it back to original prime
                                            setTimeout(100);

                                            // Simulating an error
                                            if (data >= 100 && data <= 200) {
                                                throw new RuntimeException(String.format("Simulating an error skipping prime=%s, in-between 100 and 200, while continue streaming the rest", data));
                                            }

                                            return data / 2; // set it back to original `prime` after doubling the value
                                        });
                                    })
                                    .onErrorContinue((error, data) -> System.out.println(String.format("Data=%s triggered an error=%s", data, error.getMessage())));
                        })
                ;

        if (isHotObservable) observable = observable.share();

        Function<Integer, Consumer<CompletableFuture<Integer>>> onNext =
                subscriberNbr -> promise -> promise.thenAccept(data -> doOnNext(isHotObservable, subscriberNbr, data));

        Function<Integer, Consumer<Throwable>> onError =
                subscriberNbr -> error -> doOnError(isHotObservable, subscriberNbr, error);

        Consumer<Integer> onComplete =
                subscriberNbr -> doOnComplete(isHotObservable, subscriberNbr);

        disposable1 =
                observable
                        .subscribe(
                                promise -> onNext.apply(1).accept(promise)
                                , error -> onError.apply(1).accept(error)
                                , () -> onComplete.accept(1)
                        );

        setTimeout(2000);
        disposable2 =
                observable
                        .subscribe(
                                promise -> onNext.apply(2).accept(promise)
                                , error -> onError.apply(2).accept(error)
                                , () -> onComplete.accept(2)
                        );

        setTimeout(2000);
        disposable3 =
                observable
                        .subscribe(
                                promise -> onNext.apply(3).accept(promise)
                                , error -> onError.apply(3).accept(error)
                                , () -> onComplete.accept(3)
                        );


        boolean anySubscribersStillListening;
        do {
            anySubscribersStillListening =
                    (
                            (disposable1 != null && ! disposable1.isDisposed())
                                    || (disposable2 != null && ! disposable2.isDisposed())
                                    || (disposable3 != null && ! disposable3.isDisposed())
                    );
            setTimeout(1000);
        } while (anySubscribersStillListening);

        System.out.println(
                String.format("DONE with %s Reactor Flux from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , Thread.currentThread().getName()
                ));
    }
}