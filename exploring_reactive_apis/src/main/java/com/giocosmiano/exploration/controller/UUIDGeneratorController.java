package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.service.UUIDGeneratorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Controller
@RequestMapping(value = "/uuid")
@CrossOrigin(origins = "*", allowedHeaders = "*")
public class UUIDGeneratorController {

    private final UUIDGeneratorService uuidGeneratorService;

    public UUIDGeneratorController(UUIDGeneratorService uuidGeneratorService) {
        this.uuidGeneratorService = uuidGeneratorService;
    }

    private static final Logger log = LoggerFactory.getLogger(UUIDGeneratorController.class);

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
