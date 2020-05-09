package com.giocosmiano.exploration.config;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.service.Jose4jService;
import com.giocosmiano.exploration.service.JwtService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Objects;

@SpringBootTest
@RunWith(SpringRunner.class)
@TestPropertySource("classpath:application-test.properties")
public class Jose4jConfigTests {

    private Book book;

    @Autowired private Jose4jService jose4jService;

    @Before
    public void setUp() {
        long i = 3;
        book =
                new Book(i
                        , "title" + i
                        , "isbn" + i
                        , (i * 10)
                        , null
                        , "thumbnailUrl" + i
                        , "shortDescription" + i
                        , "longDescription" + i
                        , "status" + i
                        , new ArrayList<>()
                        , new ArrayList<>()
                );
    }

    @Test
    public void generateAndParsingJweConfigsShouldWork() {
        String encodedJwe = jose4jService.generateJweConfigs(book).block();
        Mono<Book> decodedBook = jose4jService.parseJweConfigs(encodedJwe);
        StepVerifier.create(decodedBook)
                .expectNextMatches(results ->
                        results.getId() == 3 &&
                                results.getTitle().equalsIgnoreCase("title3") &&
                                results.getIsbn().equalsIgnoreCase("isbn3") &&
                                Objects.isNull(results.getPublishedDate())
                )
                .expectComplete()
                .verify();
    }
}