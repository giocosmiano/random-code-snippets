package com.giocosmiano.exploration.respository;

import com.giocosmiano.exploration.domain.Book;
import org.springframework.data.repository.query.ReactiveQueryByExampleExecutor;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface BookRepository extends
        ReactiveCrudRepository<Book, String>,
        ReactiveQueryByExampleExecutor<Book> {

    /*
     ReactiveCrudRepository provides the standard CRUD operations with reactive options ( Mono and Flux
     return types, and more)

     ReactiveQueryByExampleExecutor is a mix-in interface that introduces the Query by Example operations
     */

}
