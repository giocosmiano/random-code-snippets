package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.H2User;
import com.giocosmiano.exploration.service.H2UserService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Log4j2
@Controller
@RequestMapping(value = "/h2Users")
public class H2UserController {

    private final H2UserService h2UserService;

    public H2UserController(H2UserService h2UserService) {
        this.h2UserService = h2UserService;
    }

    @ResponseBody
    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2User> getById(@PathVariable("id") final Long id) {
        return h2UserService
                .getById(id)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/userName/{userName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2User> getById(@PathVariable("userName") final String userName) {
        return h2UserService
                .getByUsername(userName)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/counts", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<Long> getCounts() {
        return h2UserService
                .getCounts()
                ;
    }

    @ResponseBody
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<H2User> getAllUsers() {
        return h2UserService
                .getAllUsers()
                ;
    }

    // NOTE: Used for simulating streaming json
    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see usersReactorFlux.html)
    @ResponseBody
    @GetMapping(value = "/streaming", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<H2User> streamingAllUsers() {
        return h2UserService
                .getAllUsers()
                .delayElements(Duration.ofMillis(20)) // kludgy but for simulation purposes, delay each element to throttle down streaming of data to client
                .switchIfEmpty(Flux.empty())
        ;
    }

    @ResponseBody
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<H2User>> create(@RequestBody final H2User h2User) {
        return h2UserService
                .create(h2User)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2User> update(@RequestBody final H2User h2User) {
        return h2UserService
                .update(h2User)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST)))
                ;
    }

    @ResponseBody
    @DeleteMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2User> delete(@PathVariable("id") final Long id) {
        return h2UserService
                .delete(id)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                ;
    }
}
