package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.repository.BookRepository;
import io.vavr.control.Either;
import lombok.extern.log4j.Log4j2;
import org.bson.types.ObjectId;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Objects;

@Log4j2
@Service
public class BookService {

    private final BookRepository bookRepository;

    public BookService(BookRepository bookRepository) {
        this.bookRepository = bookRepository;
    }

    public Mono<Book> getById(final Long id) {
        return bookRepository.findById(id);
    }

    public Flux<Book> getByIsbn(final String id) {
        return bookRepository.findBookByIsbn(id);
    }

    public Flux<Book> getAllBooks() {
        return bookRepository.findAll();
    }

    public Flux<Book> streamingAllBooks() {
        return bookRepository.streamingAllBooks();
    }

    public Mono<Book> create(final Book book) {
        if (Objects.nonNull(book)) {
            book.setId((long) new ObjectId().getTimestamp());
            return bookRepository
                    .save(book)
                    .log("bookService.create() on log()" + book)
//                    .doOnNext(createdEntity -> log.info(" Thread " + Thread.currentThread().getName() + " created book ==> " + createdEntity))
                    ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<Book> update(final Book book) {
        if (Objects.nonNull(book) && Objects.nonNull(book.getId())) {
            return bookRepository
                    .findById(book.getId())
                    .map(oldBook -> book)
                    .flatMap(bookRepository::save)
                    .log("bookService.update() on log()" + book)
//                    .doOnNext(updatedEntity -> log.info(" Thread " + Thread.currentThread().getName() + " updated book ==> " + updatedEntity))
                    ;

        } else {
            return Mono.empty();
        }
    }

    // http://zetcode.com/all/#springboot
    // http://zetcode.com/springboot/mongodbreactive/
    // https://mkyong.com/mongodb/spring-data-mongodb-update-document/
    // https://medium.com/@nikeshshetty/5-common-mistakes-of-webflux-novices-f8eda0cd6291
    // https://www.devglan.com/spring-boot/spring-boot-mongodb-crud
    // https://www.roytuts.com/spring-boot-mongodb-functional-reactive-crud-example/
    public Mono<Book> delete(final Long id) {
        return bookRepository
                .findById(id)
                .map(Either::right)// storing the book so we can reply back to client the book is successfully deleted otherwise an error if empty
                .flatMap(either ->
                        bookRepository
                                .delete(either.get())
                                .then(Mono.just(either.get())) // returning the deleted entity by playing another Mono<Book> after the Mono<Void> completes
                )
                .log("bookService.delete() on log()" + id)
//                .doOnNext(deletedEntity -> log.info(" Thread " + Thread.currentThread().getName() + " deleted book ==> " + deletedEntity))
                ;
    }
}