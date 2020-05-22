package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.service.UUIDGeneratorService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Log4j2
@Controller
@RequestMapping(value = "/uuid")
public class UUIDGeneratorController {

    private final UUIDGeneratorService uuidGeneratorService;

    public UUIDGeneratorController(UUIDGeneratorService uuidGeneratorService) {
        this.uuidGeneratorService = uuidGeneratorService;
    }

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<String>> generateEagerRandomUUIDs(@RequestParam(name = "numberOfUUIDs", required = false) Integer numberOfUUIDs) {
        return uuidGeneratorService
                .generateEagerRandomUUIDs(numberOfUUIDs)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @GetMapping(value = "lazyCallable", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<String>> generateLazyCallableRandomUUIDs(@RequestParam(name = "numberOfUUIDs", required = false) Integer numberOfUUIDs) {
        return uuidGeneratorService
                .generateLazyCallableRandomUUIDs(numberOfUUIDs)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @GetMapping(value = "lazyDefer", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<String>> generateLazyDeferRandomUUIDs(@RequestParam(name = "numberOfUUIDs", required = false) Integer numberOfUUIDs) {
        return uuidGeneratorService
                .generateLazyDeferRandomUUIDs(numberOfUUIDs)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }
}
