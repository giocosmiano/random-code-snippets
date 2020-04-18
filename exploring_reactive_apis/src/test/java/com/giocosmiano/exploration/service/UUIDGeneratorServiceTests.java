package com.giocosmiano.exploration.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static com.giocosmiano.exploration.service.UUIDGeneratorService.*;

@SpringBootTest
@RunWith(SpringRunner.class)
public class UUIDGeneratorServiceTests {

    @Autowired private UUIDGeneratorService uuidGeneratorService;

    @Test
    public void generateRandomUUIDsShouldHaveSameUUIDs() {
        Flux<String> uuids = uuidGeneratorService.generateRandomUUIDs();
        StepVerifier.create(uuids)
                .recordWith(ArrayList::new)
                .expectNextCount(DEFAULT_NUMBER_OF_RANDOM_UUIDS)
                .consumeRecordedWith(results -> {
                    List<String> uniqueUUIDs = results.stream().distinct().collect(Collectors.toList());
                    assertThat(uniqueUUIDs).hasSize(1);
                    assertThat(results).hasSize(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
                })
                .expectComplete()
                .verify();
    }

    @Test
    public void generateCallableRandomUUIDsShouldBeRandom() {
        Flux<String> uuids = uuidGeneratorService.generateCallableRandomUUIDs();
        StepVerifier.create(uuids)
                .recordWith(ArrayList::new)
                .expectNextCount(DEFAULT_NUMBER_OF_RANDOM_UUIDS)
                .consumeRecordedWith(results -> {
                    List<String> uniqueUUIDs = results.stream().distinct().collect(Collectors.toList());
                    assertThat(uniqueUUIDs).hasSize(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
                    assertThat(results).hasSize(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
                    assertThat(results).doesNotHaveDuplicates();
                })
                .expectComplete()
                .verify();
    }

    @Test
    public void generateDeferRandomUUIDsShouldBeRandom() {
        Flux<String> uuids = uuidGeneratorService.generateDeferRandomUUIDs();
        StepVerifier.create(uuids)
                .recordWith(ArrayList::new)
                .expectNextCount(5)
                .consumeRecordedWith(results -> {
                    List<String> uniqueUUIDs = results.stream().distinct().collect(Collectors.toList());
                    assertThat(uniqueUUIDs).hasSize(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
                    assertThat(results).hasSize(DEFAULT_NUMBER_OF_RANDOM_UUIDS);
                    assertThat(results).doesNotHaveDuplicates();
                })
                .expectComplete()
                .verify();
    }
}