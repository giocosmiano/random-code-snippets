package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.H2Book;
import com.giocosmiano.exploration.service.H2BookService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
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
        model.addAttribute("pageTitle", "Streaming Simulation using RxJS and Oboe.js in the UI while Reactor Flux, with Non-Reactive H2 DB-driver, on the back-end");
        return Mono.just("booksReactorFlux");
    }

    @ResponseBody
    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2Book> getById(@PathVariable("id") final Long id) {
        return bookService
                .getById(id)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<H2Book> getByIsbn(@PathVariable("id") final String id) {
        return bookService
                .getByIsbn(id)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/counts", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<Long> getCounts() {
        return bookService
                .getCounts()
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
    public Mono<H2Book> create(@RequestBody final H2Book book) {
        return bookService
                .create(book)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST)))
                ;
    }

    @ResponseBody
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2Book> update(@RequestBody final H2Book book) {
        return bookService
                .update(book)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST)))
                ;
    }

    @ResponseBody
    @DeleteMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<H2Book> delete(@PathVariable("id") final Long id) {
        return bookService
                .delete(id)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND)))
                ;
    }
}
