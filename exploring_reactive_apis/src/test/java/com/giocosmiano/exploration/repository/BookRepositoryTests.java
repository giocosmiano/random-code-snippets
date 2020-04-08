package com.giocosmiano.exploration.repository;

import com.giocosmiano.exploration.domain.Book;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;

// https://docs.spring.io/spring-boot/docs/2.0.0.M2/reference/htmlsingle/#boot-features-test-scope-dependencies
@DataMongoTest
@RunWith(SpringRunner.class)
@Transactional(propagation = Propagation.NOT_SUPPORTED)
public class BookRepositoryTests {

    // This `SinglePlatformTransactionManagerConfiguration` is to resolve the exception error
    // Caused by: org.springframework.beans.factory.NoSuchBeanDefinitionException:
    // No qualifying bean of type 'org.springframework.transaction.PlatformTransactionManager' available: expected at least 1 bean which qualifies as autowire candidate. Dependency annotations: {}
    // See --> https://github.com/spring-projects/spring-boot/blob/master/spring-boot-project/spring-boot-autoconfigure/src/test/java/org/springframework/boot/autoconfigure/transaction/TransactionAutoConfigurationTests.java
    @EnableAutoConfiguration
    @Configuration
    static class SinglePlatformTransactionManagerConfiguration {
        @Bean
        PlatformTransactionManager transactionManager() {
            return Mockito.mock(PlatformTransactionManager.class);
        }
    }

    // see https://docs.spring.io/spring-boot/docs/2.1.5.RELEASE/reference/html/boot-features-testing.html
    @Autowired BookRepository bookRepository;
    @Autowired MongoOperations mongoOperations;

    @Before
    public void setUp() {
        mongoOperations.dropCollection(Book.class);
        Flux.range(1, 4)
                .doOnNext(i ->
                        bookRepository.save(
                                new Book(Long.valueOf(i)
                                        , "title" + i
                                        , "isbn" + i
                                        , Long.valueOf(i * 10)
                                        , new Date()
                                        , "thumbnailUrl" + i
                                        , "shortDescription" + i
                                        , "longDescription" + i
                                        , "status" + i
                                        , new ArrayList<>()
                                        , new ArrayList<>()
                                )
                        ).block()
                )
                .subscribe();

        // creating duplicate isbn
        long i = 3;
        bookRepository.save(
                new Book(i * 10
                        , "title" + i
                        , "isbn" + i
                        , (i * 10)
                        , new Date()
                        , "thumbnailUrl" + i
                        , "shortDescription" + i
                        , "longDescription" + i
                        , "status" + i
                        , new ArrayList<>()
                        , new ArrayList<>()
                )).block();
    }

    @Test
    public void findAllShouldWork() {
        Flux<Book> books = bookRepository.findAll();
        StepVerifier.create(books)
                .recordWith(ArrayList::new)
                .expectNextCount(5)
                .consumeRecordedWith(results -> {
                    assertThat(results).hasSize(5);
                    assertThat(results)
                            .extracting(Book::getIsbn)
                            .contains(
                                    "isbn1"
                                    , "isbn2"
                                    , "isbn3"
                                    , "isbn4"
                            );
                })
                .expectComplete()
                .verify();
    }

    @Test
    public void findBookByIsbnShouldWork() {
        Flux<Book> books = bookRepository.findBookByIsbn("isbn3");
        StepVerifier.create(books)
                .expectNextMatches(results -> results.getIsbn().equalsIgnoreCase("isbn3"))
                .expectNextMatches(results -> results.getIsbn().equalsIgnoreCase("isbn3"))
                .expectComplete()
                .verify();
    }

}