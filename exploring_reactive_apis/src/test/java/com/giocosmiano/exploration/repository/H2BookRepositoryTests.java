package com.giocosmiano.exploration.repository;

import com.giocosmiano.exploration.domain.H2Book;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;

import java.util.ArrayList;
import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;

@DataJpaTest
@RunWith(SpringRunner.class)
@Transactional(propagation = Propagation.NOT_SUPPORTED)
public class H2BookRepositoryTests {

    // see https://docs.spring.io/spring-boot/docs/2.1.5.RELEASE/reference/html/boot-features-testing.html
    @Autowired H2BookRepository bookRepository;

    @Before
    public void setUp() {
        bookRepository.deleteAll();
        Flux.range(1, 4)
                .doOnNext(i -> {
                            H2Book h2Book = new H2Book();
                            h2Book.setId(Long.valueOf(i));
                            h2Book.setTitle("title" + i);
                            h2Book.setIsbn("isbn" + i);
                            h2Book.setPageCount(Long.valueOf(i * 10));
                            h2Book.setPublishedDate(new Date());
                            h2Book.setThumbnailUrl("thumbnailUrl" + i);
                            h2Book.setShortDescription("shortDescription" + i);
                            h2Book.setLongDescription("longDescription" + i);
                            h2Book.setStatus("status" + i);
                            h2Book.setAuthors(new ArrayList<>());
                            h2Book.setCategories(new ArrayList<>());
                            bookRepository.save(h2Book);
                })
                .subscribe();

        // creating duplicate isbn
        long i = 3;
        H2Book h2Book = new H2Book();
        h2Book.setId(i * 10); // new id
        h2Book.setTitle("title" + i);
        h2Book.setIsbn("isbn" + i);
        h2Book.setPageCount(i * 10);
        h2Book.setPublishedDate(new Date());
        h2Book.setThumbnailUrl("thumbnailUrl" + i);
        h2Book.setShortDescription("shortDescription" + i);
        h2Book.setLongDescription("longDescription" + i);
        h2Book.setStatus("status" + i);
        h2Book.setAuthors(new ArrayList<>());
        h2Book.setCategories(new ArrayList<>());
        bookRepository.save(h2Book);
    }

    @Test
    public void findAllShouldWork() {
        Iterable<H2Book> books = bookRepository.findAll();
        assertThat(books).hasSize(5);
    }

    @Test
    public void findBookByIsbnShouldWork() {
        Iterable<H2Book> books = bookRepository.findByIsbn("isbn3");
        assertThat(books).hasSize(2);
    }

}