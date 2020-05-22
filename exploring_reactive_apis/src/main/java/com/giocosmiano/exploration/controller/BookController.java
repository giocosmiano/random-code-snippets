package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.service.BookService;
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
@RequestMapping(value = "/books")
public class BookController {

    private final BookService bookService;

    public BookController(BookService bookService) {
        this.bookService = bookService;
    }

    // see these Thymeleaf references when accessing data and javascript variables
    // https://www.thymeleaf.org/doc/tutorials/2.1/usingthymeleaf.html#script-inlining-javascript-and-dart
    // https://www.thymeleaf.org/doc/articles/springmvcaccessdata.html
    @GetMapping
    public Mono<String> index(Model model) {
//        model.addAttribute("restEndpoint", "http://localhost:9080/books/streaming");
        model.addAttribute("restEndpoint", "/books/streaming");
        model.addAttribute("repository", "mongo");
        model.addAttribute("pageTitle", "Streaming Simulation on RxJS Observables and Oboe.js in the UI while Reactive Mongo and Reactor Flux from ReST");
        return Mono.just("booksReactorFlux");
    }

    @ResponseBody
    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> getById(@PathVariable("id") final Long id) {
        return bookService
                .getById(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<Book>> getByIsbn(@PathVariable("id") final String id) {
        return bookService
                .getByIsbn(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Flux.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/all", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<Book> getAllBooks() {
        return bookService
                .getAllBooks()
                ;
    }

    // NOTE: Used for simulating streaming json
    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see booksReactorFlux.html)
    @ResponseBody
    @GetMapping(value = "/streaming", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<ResponseEntity<Book>> streamingAllBooks() {
        return bookService
                .streamingAllBooks()
                .delayElements(Duration.ofMillis(20)) // kludgy but for simulation purposes, delay each element to throttle down streaming of data to client
                .map(ResponseEntity::ok)
                .switchIfEmpty(Flux.just(ResponseEntity.noContent().build()))
        ;
    }

    @ResponseBody
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> create(@RequestBody final Book book) {
        return bookService
                .create(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> update(@RequestBody final Book book) {
        return bookService
                .update(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @DeleteMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> delete(@PathVariable("id") final Long id) {
        return bookService
                .delete(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }
}
