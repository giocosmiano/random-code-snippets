package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.config.Jose4jConfig;
import com.giocosmiano.exploration.domain.Book;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Objects;

@Log4j2
@Service
public class Jose4jService {

    @Value("${jwtSecretKey:jwtSecretKey}")
    private String jwtSecretKey;

    public Mono<String> generateJweConfigs(final Book book) {
        return Mono.fromCallable(() -> {
            String encodedJweBook = null;
            if (Objects.nonNull(book)) {
                encodedJweBook = Jose4jConfig.generateJweConfigs(jwtSecretKey, book);
            }
            return encodedJweBook;
        });

        // OR using Mono.defer()
//        return Mono.defer(() -> {
//            String encodedJweBook = null;
//            if (Objects.nonNull(book)) {
//                encodedJweBook = Jose4jConfig.generateJweConfigs(jwtSecretKey, book);
//            }
//            return Mono.justOrEmpty(encodedJweBook);
//        });
    }

    public Mono<Book> parseJweConfigs(final String encodedJweBook) {
        return Mono.fromCallable(() -> {
            Book book = null;
            if (StringUtils.isNoneEmpty(encodedJweBook)) {
                book = Jose4jConfig.parseJweConfigs(jwtSecretKey, encodedJweBook, Book.class);
            }
            return book;
        });

        // OR using Mono.defer()
//        return Mono.defer(() -> {
//            Book book = null;
//            if (StringUtils.isNoneEmpty(encodedJweBook)) {
//                book = Jose4jConfig.parseJweConfigs(jwtSecretKey, encodedJweBook, Book.class);
//            }
//            return Mono.justOrEmpty(book);
//        });
    }
}