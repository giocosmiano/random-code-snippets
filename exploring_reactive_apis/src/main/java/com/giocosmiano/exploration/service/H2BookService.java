package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.aspect.LoggingAspect;
import com.giocosmiano.exploration.domain.H2Book;
import com.giocosmiano.exploration.repository.H2BookRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

import java.util.Objects;

@Log4j2
@Service
public class H2BookService extends LoggingWithContextService {

    private final Scheduler jdbcScheduler;
    private final TransactionTemplate transactionTemplate;
    private final H2BookRepository bookRepository;

    public H2BookService(
            @Qualifier("jdbcScheduler") Scheduler jdbcScheduler
            , TransactionTemplate transactionTemplate
            , H2BookRepository bookRepository) {
        this.jdbcScheduler = jdbcScheduler;
        this.transactionTemplate = transactionTemplate;
        this.bookRepository = bookRepository;
    }

    public Mono<H2Book> getById(final Long id) {
        return Mono
                .defer(() ->
                        // Exploring: Thread-local state availability in reactive services
                        // https://kamilszymanski.github.io/thread-local-state-availability-in-reactive-services/
                        // https://dzone.com/articles/thread-local-state-availability-in-reactive-servic
                        Mono
                        .justOrEmpty(bookRepository.findById(id))
                        .zipWith(Mono.subscriberContext()) // capture the context created from controller thru LoggingAspect.loggingReactorWithAddedContext()
                        .flatMap(flatMapMonoToLogWithContext(this::loggingMonoOnNextWithContext))
                ) // using `defer` to re-evaluate the lambda for each request thus making lazy IO call
                .doOnNext(entity -> log.info(" Thread " + Thread.currentThread().getName() + " get book ==> " + entity))
                .subscribeOn(jdbcScheduler) // while running the request on different thread-pool
                ;
    }

    public Flux<H2Book> getByIsbn(final String id) {
        return Flux
                .defer(() -> Flux.fromIterable(bookRepository.findByIsbn(id))) // using `defer` to re-evaluate the lambda for each request thus making lazy IO call
                .doOnNext(entity -> log.info(" Thread " + Thread.currentThread().getName() + " get book ==> " + entity))
                .subscribeOn(jdbcScheduler) // while running the request on different thread-pool
                ;
    }

    public Flux<H2Book> getAllBooks() {
        return Flux
                .defer(() -> Flux.fromIterable(bookRepository.findAll())) // using `defer` to re-evaluate the lambda for each request thus making lazy IO call
                .doOnNext(entity -> log.info(" Thread " + Thread.currentThread().getName() + " get book ==> " + entity))
                .subscribeOn(jdbcScheduler) // while running the request on different thread-pool
                ;
    }

    public Mono<H2Book> create(final H2Book book) {
        if (Objects.nonNull(book)) {
            book.setId(null);
            return Mono
                    .fromCallable(() -> transactionTemplate.execute(status -> bookRepository.save(book)))
                    .log("bookService.create() on log()" + book)
                    .doOnNext(createdEntity -> log.info(" Thread " + Thread.currentThread().getName() + " created book ==> " + createdEntity))
                    .subscribeOn(jdbcScheduler) // running the request on different thread-pool
            ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<H2Book> update(final H2Book book) {
        if (Objects.nonNull(book) && Objects.nonNull(book.getId())) {
            return Mono
                    .fromCallable(() -> transactionTemplate.execute(status -> {
                        H2Book oldBook = bookRepository.findById(book.getId()).orElse(null);
                        if (Objects.isNull(oldBook)) {
                            return oldBook;
                        }
                        return bookRepository.save(book);
                    }))
                    .log("bookService.update() on log()" + book)
                    .doOnNext(updatedEntity -> log.info(" Thread " + Thread.currentThread().getName() + " updated book ==> " + updatedEntity))
                    .subscribeOn(jdbcScheduler) // running the request on different thread-pool
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
    public Mono<H2Book> delete(final Long id) {
        return Mono
                .fromCallable(() -> transactionTemplate.execute(status -> {
                    H2Book oldBook = bookRepository.findById(id).orElse(null);
                    if (Objects.isNull(oldBook)) {
                        return oldBook;
                    }
                    bookRepository.delete(oldBook);
                    return oldBook;
                }))
                .log("bookService.delete() on log()" + id)
                .doOnNext(deletedEntity -> log.info(" Thread " + Thread.currentThread().getName() + " deleted book ==> " + deletedEntity))
                .subscribeOn(jdbcScheduler) // running the request on different thread-pool
                ;
    }
}