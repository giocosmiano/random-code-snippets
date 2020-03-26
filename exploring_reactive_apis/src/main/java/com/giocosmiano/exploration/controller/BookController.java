package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.respository.BookRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Example;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping(value = "/books")
public class BookController {

    private BookRepository bookRepository;

    public BookController(BookRepository bookRepository) {
        this.bookRepository = bookRepository;
    }

    private static final Logger log = LoggerFactory.getLogger(BookController.class);

    private static final String API_BASE_PATH = "/api";

    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    Mono<Book> bookById(@PathVariable("id") String id) {
        return bookRepository.findById(id);
    }

    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    Mono<Book> bookByIsbn(@PathVariable("id") String id) {
        Book book = new Book();
        book.setIsbn(id);
        return bookRepository.findOne(Example.of(book));
    }

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    Flux<Book> books() {
        return bookRepository.findAll();
    }

    @PostMapping
    Mono<Void> create(@RequestBody Flux<Book> books) {
        return books
                .map(book -> {
                    log.info("We'll be saving " + book +
                            " to a Reactive database soon!");
                    return book;
                })
                .then();
    }

}