package com.giocosmiano.exploration.repository;

import com.giocosmiano.exploration.domain.Book;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.repository.query.ReactiveQueryByExampleExecutor;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface BookRepository extends
        ReactiveCrudRepository<Book, Long>,
        ReactiveQueryByExampleExecutor<Book> {

    /*
     ReactiveCrudRepository provides the standard CRUD operations with reactive options ( Mono and Flux
     return types, and more)

     ReactiveQueryByExampleExecutor is a mix-in interface that introduces the Query by Example operations
     */

    // https://www.devglan.com/spring-boot/spring-data-mongodb-queries
    @Query("{'isbn' : ?0}")
    Mono<Book> findBookByIsbn(String id);

    @Query(value = "{}", fields = "{'shortDescription' : 0, 'longDescription' : 0, 'thumbnailUrl' : 0}") // all fields excluding short/long descriptions, thumbnailUrl
    Flux<Book> streamingAllBooks();
}
