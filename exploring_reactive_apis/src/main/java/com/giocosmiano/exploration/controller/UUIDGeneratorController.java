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
    public Flux<ResponseEntity<String>> generateRandomUUID(@RequestParam(name = "numberOfUUIDs", required = false) Integer numberOfUUIDs) {
        return uuidGeneratorService
                .generateRandomUUIDs(numberOfUUIDs)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @GetMapping(value = "callable", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<String>> generateCallableRandomUUIDs(@RequestParam(name = "numberOfUUIDs", required = false) Integer numberOfUUIDs) {
        return uuidGeneratorService
                .generateCallableRandomUUIDs(numberOfUUIDs)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @GetMapping(value = "defer", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<String>> generateDeferRandomUUID(@RequestParam(name = "numberOfUUIDs", required = false) Integer numberOfUUIDs) {
        return uuidGeneratorService
                .generateDeferRandomUUIDs(numberOfUUIDs)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }
}
