package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.repository.BookRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
public class BookService {

    protected static final Logger log = LoggerFactory.getLogger(BookService.class);

    private BookRepository bookRepository;

    public BookService(BookRepository bookRepository) {
        this.bookRepository = bookRepository;
    }

    public Mono<Book> getById(Long id) {
        return bookRepository.findById(id);
    }

    public Mono<Book> getByIsbn(String id) {
        return bookRepository.findBookByIsbn(id);
    }

    public Flux<Book> getAllBooks() {
        return bookRepository.findAll();
    }

    public Flux<Book> streamingAllBooks() {
        return bookRepository.streamingAllBooks();
    }

    public Mono<Void> create(Flux<Book> books) {

        return books
                .map(book -> {
                    log.info("We'll be saving " + book +
                            " to a Reactive database soon!");
                    return book;
                })
                .then();
    }
}