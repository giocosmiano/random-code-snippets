package com.giocosmiano.exploration.config;

import com.giocosmiano.exploration.domain.Book;
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

@SpringBootTest
@RunWith(SpringRunner.class)
@TestPropertySource("classpath:application-test.properties")
public class JwtConfigTests {

    private Book book;
    private String encodedBook = "eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiJjNGE5YTQ3MC00ZDdmLTQwMWItOWFlYi1jNTMwMmU1ZDU4MTAiLCJzdWIiOiJ1c2VySWRfMTIzIiwiYXVkIjoiYXVkaWVuY2UiLCJpc3MiOiJzYW1wbGVJc3N1ZXIiLCJpYXQiOjE1ODY1MzUzNDcsImV4cCI6MTU4NjYyMTc0NywiandzQm9keSI6IntcImlkXCI6MyxcInRpdGxlXCI6XCJ0aXRsZTNcIixcImlzYm5cIjpcImlzYm4zXCIsXCJwYWdlQ291bnRcIjozMCxcInB1Ymxpc2hlZERhdGVcIjpudWxsLFwidGh1bWJuYWlsVXJsXCI6XCJ0aHVtYm5haWxVcmwzXCIsXCJzaG9ydERlc2NyaXB0aW9uXCI6XCJzaG9ydERlc2NyaXB0aW9uM1wiLFwibG9uZ0Rlc2NyaXB0aW9uXCI6XCJsb25nRGVzY3JpcHRpb24zXCIsXCJzdGF0dXNcIjpcInN0YXR1czNcIixcImF1dGhvcnNcIjpbXSxcImNhdGVnb3JpZXNcIjpbXX0ifQ.Eda_GpBDHWfzhl6YPBt92h1ZUSaCJPhrFgK14as1i_o";

    @Autowired private JwtService jwtService;

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

    // Tests are NOT going to work because jwtConfig is setting the Id randomly and issuedAt/expiration are also dynamically generated
//    @Test
    public void generateJwtConfigsShouldWork() {
        Mono<String> encodedJwt = jwtService.generateJwtConfigs(book);
        StepVerifier.create(encodedJwt)
                .expectNext(encodedBook)
                .expectComplete()
                .verify();
    }

    // Tests are NOT going to work because jwtConfig is setting the Id randomly and issuedAt/expiration are also dynamically generated
//    @Test
    public void findBookByIsbnShouldWork() {
        String encodedJwt = "";
        Mono<Book> decodedBook = jwtService.parseJwtConfigs(encodedJwt);
        StepVerifier.create(decodedBook)
                .expectNextMatches(results -> results.getIsbn().equalsIgnoreCase("isbn3"))
                .expectComplete()
                .verify();
    }

}