package com.giocosmiano.exploration.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

@Service
public class UUIDGeneratorService {

    public static final Integer DEFAULT_NUMBER_OF_RANDOM_UUIDS = 5;
    protected static final Logger log = LoggerFactory.getLogger(UUIDGeneratorService.class);

    private final Function<Integer, Flux<String>> getRandomUUIDs =
            numberOfUUIDs -> {
                AtomicReference<List<String>> atomicReference = new AtomicReference<>();
                atomicReference.set(new ArrayList<>());
                Mono<String> mono = Mono.just(UUID.randomUUID().toString());
                IntStream.range(0, numberOfUUIDs)
                        .forEach(i -> mono.subscribe(uuid -> atomicReference.get().add(uuid)));
                return Flux.fromIterable(atomicReference.get());
            };

    private final Function<Integer, Flux<String>> getCallableRandomUUIDs =
            numberOfUUIDs -> {
                AtomicReference<List<String>> atomicReference = new AtomicReference<>();
                atomicReference.set(new ArrayList<>());
                Mono<String> mono = Mono.fromCallable(() -> UUID.randomUUID().toString());
                IntStream.range(0, numberOfUUIDs)
                        .forEach(i -> mono.subscribe(uuid -> atomicReference.get().add(uuid)));
                return Flux.fromIterable(atomicReference.get());
            };

    private final Function<Integer, Flux<String>> getDeferRandomUUIDs =
            numberOfUUIDs -> {
                AtomicReference<List<String>> atomicReference = new AtomicReference<>();
                atomicReference.set(new ArrayList<>());
                Mono<String> mono = Mono.defer(() -> Mono.just(UUID.randomUUID().toString()));
                IntStream.range(0, numberOfUUIDs)
                        .forEach(i -> mono.subscribe(uuid -> atomicReference.get().add(uuid)));
                return Flux.fromIterable(atomicReference.get());
            };

    public Flux<String> generateRandomUUIDs() {
        return generateRandomUUIDs(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
    }

    public Flux<String> generateRandomUUIDs(final Integer numberOfUUIDs) {
        Integer nbrOfUUIDs = Optional.ofNullable(numberOfUUIDs).orElseGet(() -> DEFAULT_NUMBER_OF_RANDOM_UUIDS);
        return getRandomUUIDs.apply(nbrOfUUIDs);
    }

    public Flux<String> generateCallableRandomUUIDs() {
        return generateCallableRandomUUIDs(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
    }

    public Flux<String> generateCallableRandomUUIDs(final Integer numberOfUUIDs) {
        Integer nbrOfUUIDs = Optional.ofNullable(numberOfUUIDs).orElseGet(() -> DEFAULT_NUMBER_OF_RANDOM_UUIDS);
        return getCallableRandomUUIDs.apply(nbrOfUUIDs);
    }

    public Flux<String> generateDeferRandomUUIDs() {
        return generateDeferRandomUUIDs(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
    }

    public Flux<String> generateDeferRandomUUIDs(final Integer numberOfUUIDs) {
        Integer nbrOfUUIDs = Optional.ofNullable(numberOfUUIDs).orElseGet(() -> DEFAULT_NUMBER_OF_RANDOM_UUIDS);
        return getDeferRandomUUIDs.apply(nbrOfUUIDs);
    }
}