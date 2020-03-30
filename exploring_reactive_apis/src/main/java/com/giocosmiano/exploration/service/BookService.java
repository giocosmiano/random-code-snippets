package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.repository.BookRepository;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Objects;

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

    public Flux<Book> getByIsbn(String id) {
        return bookRepository.findBookByIsbn(id);
    }

    public Flux<Book> getAllBooks() {
        return bookRepository.findAll();
    }

    public Flux<Book> streamingAllBooks() {
        return bookRepository.streamingAllBooks();
    }

    public Mono<Book> create(Book book) {
        if (Objects.nonNull(book)) {
            book.setId(new ObjectId().getTime());
            return bookRepository
                    .save(book)
                    .log("bookService.create() on log()" + book)
                    .doOnSuccess(createdEntity -> log.info(" Thread " + Thread.currentThread().getName() + " created book ==> " + createdEntity))
                    ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<Book> update(Book book) {
        if (Objects.nonNull(book) && Objects.nonNull(book.getId())) {
            return bookRepository
                    .save(book)
                    .log("bookService.update() on log()" + book)
                    .doOnSuccess(updatedEntity -> log.info(" Thread " + Thread.currentThread().getName() + " updated book ==> " + updatedEntity))
                    ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<Void> delete(Long id) {
        return bookRepository
                .deleteById(id)
                .log("bookService.delete() on log()" + id)
                .doOnSuccess(deletedEntity -> log.info(" Thread " + Thread.currentThread().getName() + " deleted book with ID ==> " + id))
                ;
    }
}