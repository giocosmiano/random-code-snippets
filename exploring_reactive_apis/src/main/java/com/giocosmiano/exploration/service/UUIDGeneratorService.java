package com.giocosmiano.exploration.service;

import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.IntStream;

@Log4j2
@Service
public class UUIDGeneratorService {

    public static final Integer DEFAULT_NUMBER_OF_RANDOM_UUIDS = 5;

    private final Function<Integer, Flux<String>> getEagerRandomUUIDs =
            numberOfUUIDs -> {
                AtomicReference<List<String>> atomicReference = new AtomicReference<>();
                atomicReference.set(new ArrayList<>());
                Mono<String> mono = Mono.just(UUID.randomUUID().toString()); // using eager Mono.just()
                IntStream.range(0, numberOfUUIDs)
                        .forEach(i -> mono.subscribe(uuid -> atomicReference.get().add(uuid)));
                return Flux.fromIterable(atomicReference.get());
            };

    private final Function<Integer, Flux<String>> getLazyCallableRandomUUIDs =
            numberOfUUIDs -> {
                AtomicReference<List<String>> atomicReference = new AtomicReference<>();
                atomicReference.set(new ArrayList<>());
                Mono<String> mono = Mono.fromCallable(() -> UUID.randomUUID().toString()); // using lazy Mono.fromCallable()
                IntStream.range(0, numberOfUUIDs)
                        .forEach(i -> mono.subscribe(uuid -> atomicReference.get().add(uuid)));
                return Flux.fromIterable(atomicReference.get());
            };

    private final Function<Integer, Flux<String>> getLazyDeferRandomUUIDs =
            numberOfUUIDs -> {
                AtomicReference<List<String>> atomicReference = new AtomicReference<>();
                atomicReference.set(new ArrayList<>());
                Mono<String> mono = Mono.defer(() -> Mono.just(UUID.randomUUID().toString())); // using lazy Mono.defer()
                IntStream.range(0, numberOfUUIDs)
                        .forEach(i -> mono.subscribe(uuid -> atomicReference.get().add(uuid)));
                return Flux.fromIterable(atomicReference.get());
            };

    public Flux<String> generateEagerRandomUUIDs() {
        return generateEagerRandomUUIDs(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
    }

    public Flux<String> generateEagerRandomUUIDs(final Integer numberOfUUIDs) {
        Integer nbrOfUUIDs = Optional.ofNullable(numberOfUUIDs).orElseGet(() -> DEFAULT_NUMBER_OF_RANDOM_UUIDS);
        return getEagerRandomUUIDs.apply(nbrOfUUIDs);
    }

    public Flux<String> generateLazyCallableRandomUUIDs() {
        return generateLazyCallableRandomUUIDs(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
    }

    public Flux<String> generateLazyCallableRandomUUIDs(final Integer numberOfUUIDs) {
        Integer nbrOfUUIDs = Optional.ofNullable(numberOfUUIDs).orElseGet(() -> DEFAULT_NUMBER_OF_RANDOM_UUIDS);
        return getLazyCallableRandomUUIDs.apply(nbrOfUUIDs);
    }

    public Flux<String> generateLazyDeferRandomUUIDs() {
        return generateLazyDeferRandomUUIDs(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
    }

    public Flux<String> generateLazyDeferRandomUUIDs(final Integer numberOfUUIDs) {
        Integer nbrOfUUIDs = Optional.ofNullable(numberOfUUIDs).orElseGet(() -> DEFAULT_NUMBER_OF_RANDOM_UUIDS);
        return getLazyDeferRandomUUIDs.apply(nbrOfUUIDs);
    }
}