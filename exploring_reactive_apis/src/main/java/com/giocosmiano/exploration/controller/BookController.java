package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.service.BookService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Controller
@RequestMapping(value = "/books")
@CrossOrigin(origins = "*", allowedHeaders = "*")
public class BookController {

    private BookService bookService;

    public BookController(BookService bookService) {
        this.bookService = bookService;
    }

    private static final Logger log = LoggerFactory.getLogger(BookController.class);

    @ResponseBody
    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> getById(@PathVariable("id") Long id) {
        return bookService
                .getById(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<ResponseEntity<Book>> getByIsbn(@PathVariable("id") String id) {
        return bookService
                .getByIsbn(id)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Flux.just(ResponseEntity.notFound().build()))
                ;
    }

    @ResponseBody
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<Book> getAllBooks() {
        return bookService
                .getAllBooks()
                ;
    }

    // NOTE: Used for simulating streaming json
    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see books_reactor_flux.html)
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
    public Mono<ResponseEntity<Book>> create(@RequestBody Book book) {
        return bookService
                .create(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> update(@RequestBody Book book) {
        return bookService
                .update(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    @ResponseBody
    @DeleteMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Void>> delete(@PathVariable("id") Long id) {
        return bookService
                .delete(id)
                .map(ResponseEntity::ok)
                ;
    }

    @GetMapping(value = "/view")
    public Mono<String> index(Model model) {
        model.addAttribute("books", bookService.streamingAllBooks());
        return Mono.just("redirect:/books_reactor_flux");
    }
}
