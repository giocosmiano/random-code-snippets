package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.service.BookService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@RestController
@RequestMapping(value = "/books")
public class BookController {

    private BookService bookService;

    public BookController(BookService bookService) {
        this.bookService = bookService;
    }

    private static final Logger log = LoggerFactory.getLogger(BookController.class);

    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<Book> getById(@PathVariable("id") String id) {
        return bookService.getById(Long.valueOf(id));
    }

    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<Book> getByIsbn(@PathVariable("id") String id) {
        return bookService.getByIsbn(id);
    }

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<Book> getAllBooks() {
        return bookService.getAllBooks();
    }

    @GetMapping(value = "/streaming", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<Book> streamingAllBooks() {
        return bookService
                .streamingAllBooks()
                .delayElements(Duration.ofMillis(10)) // delay each element to throttle down streaming of data
        ;
    }

    @PostMapping
    public Mono<Void> create(@RequestBody Flux<Book> books) {
        return bookService.create(books)
                .then();
    }
}
