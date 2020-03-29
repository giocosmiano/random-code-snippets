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
    public Mono<ResponseEntity<Book>> getById(@PathVariable("id") String id) {
        return bookService
                .getById(Long.valueOf(id))
                .map(ResponseEntity::ok)
                ;
    }

    @ResponseBody
    @GetMapping(value = "/isbn/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> getByIsbn(@PathVariable("id") String id) {
        return bookService
                .getByIsbn(id)
                .map(ResponseEntity::ok)
                ;
    }

    @ResponseBody
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<Book> getAllBooks() {
        return bookService
                .getAllBooks()
                ;
    }

    @ResponseBody
    @GetMapping(value = "/streaming", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<ResponseEntity<Book>> streamingAllBooks() {
        return bookService
                .streamingAllBooks()
                .delayElements(Duration.ofMillis(5)) // delay each element to throttle down streaming of data to client
                .map(ResponseEntity::ok)
        ;
    }

    @ResponseBody
    @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<Void> create(@RequestBody Flux<Book> books) {
        return bookService.create(books)
                .then();
    }

    @GetMapping(value = "/view")
    public Mono<String> index(Model model) {
        model.addAttribute("books", bookService.streamingAllBooks());
        return Mono.just("redirect:/books_reactor_flux");
    }
}
