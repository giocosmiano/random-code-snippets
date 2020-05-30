package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.User;
import com.giocosmiano.exploration.service.UserService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Log4j2
@Controller
@RequestMapping(value = "/users")
public class UserController {

    private final UserService userService;

    public UserController(UserService userService) {
        this.userService = userService;
    }

    @ResponseBody
    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<User>> getById(@PathVariable("id") final Long id) {
        return userService
                .getById(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<User> getAllUsers() {
        return userService
                .getAllUsers()
                ;
    }

    // NOTE: Used for simulating streaming json
    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see usersReactorFlux.html)
    @ResponseBody
    @GetMapping(value = "/streaming", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<User> streamingAllUsers() {
        return userService
                .getAllUsers()
                .delayElements(Duration.ofMillis(20)) // kludgy but for simulation purposes, delay each element to throttle down streaming of data to client
                .switchIfEmpty(Flux.empty())
        ;
    }

    @ResponseBody
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<User>> create(@RequestBody final User user) {
        return userService
                .create(user)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<User>> update(@RequestBody final User user) {
        return userService
                .update(user)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @DeleteMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<User>> delete(@PathVariable("id") final Long id) {
        return userService
                .delete(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }
}
