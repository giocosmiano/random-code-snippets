package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.H2Book;
import com.giocosmiano.exploration.service.H2BookService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Log4j2
@Controller
@RequestMapping(value = "/h2Books")
public class H2BookController {

    private final H2BookService bookService;

    public H2BookController(H2BookService bookService) {
        this.bookService = bookService;
    }

    // see these Thymeleaf references when accessing data and javascript variables
    // https://www.thymeleaf.org/doc/tutorials/2.1/usingthymeleaf.html#script-inlining-javascript-and-dart
    // https://www.thymeleaf.org/doc/articles/springmvcaccessdata.html
    @GetMapping
    public Mono<String> index(Model model) {
//        model.addAttribute("restEndpoint", "http://localhost:9080/h2Books/streaming");
        model.addAttribute("restEndpoint", "/h2Books/streaming");
        model.addAttribute("repository", "h2Books");
        model.addAttribute("pageTitle", "Streaming Simulation on RxJS Observables and Oboe.js in the UI while H2 (non-reactive DB) and Reactor Flux from ReST");
        return Mono.just("booksReactorFlux");
    }

    @ResponseBody
    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<H2Book>> getById(@PathVariable("id") final Long id) {
        return bookService
                .getById(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<H2Book>> getByIsbn(@PathVariable("id") final String id) {
        return bookService
                .getByIsbn(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Flux.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/all", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<H2Book> getAllBooks() {
        return bookService
                .getAllBooks()
                ;
    }

    // NOTE: Used for simulating streaming json
    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see booksReactorFlux.html)
    @ResponseBody
    @GetMapping(value = "/streaming", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<ResponseEntity<H2Book>> streamingAllH2Books() {
        return bookService
                .getAllBooks()
                .delayElements(Duration.ofMillis(20)) // kludgy but for simulation purposes, delay each element to throttle down streaming of data to client
                .map(ResponseEntity::ok)
                .switchIfEmpty(Flux.just(ResponseEntity.noContent().build()))
        ;
    }

    @ResponseBody
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<H2Book>> create(@RequestBody final H2Book book) {
        return bookService
                .create(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<H2Book>> update(@RequestBody final H2Book book) {
        return bookService
                .update(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @DeleteMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<H2Book>> delete(@PathVariable("id") final Long id) {
        return bookService
                .delete(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }
}
