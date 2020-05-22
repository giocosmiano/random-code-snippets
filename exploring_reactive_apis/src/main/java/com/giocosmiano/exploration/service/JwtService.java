package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.config.JwtConfig;
import com.giocosmiano.exploration.domain.Book;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Objects;

@Log4j2
@Service
public class JwtService {

    @Value("${jwtSecretKey:jwtSecretKey}")
    private String jwtSecretKey;

    public Mono<String> generateJwtConfigs(final Book book) {
        return Mono.fromCallable(() -> {
            String encodedJwtBook = null;
            if (Objects.nonNull(book)) {
                encodedJwtBook = JwtConfig.generateJwtConfigs(jwtSecretKey, book);
            }
            return encodedJwtBook;
        });

        // OR using Mono.defer()
//        return Mono.defer(() -> {
//            String encodedJwtBook = null;
//            if (Objects.nonNull(book)) {
//                encodedJwtBook = JwtConfig.generateJwtConfigs(jwtSecretKey, book);
//            }
//            return Mono.justOrEmpty(encodedJwtBook);
//        });
    }

    public Mono<Book> parseJwtConfigs(final String encodedJwtBook) {
        return Mono.fromCallable(() -> {
            Book book = null;
            if (StringUtils.isNoneEmpty(encodedJwtBook)) {
                book = JwtConfig.parseJwtConfigs(jwtSecretKey, encodedJwtBook, Book.class);
            }
            return book;
        });

        // OR using Mono.defer()
//        return Mono.defer(() -> {
//            Book book = null;
//            if (StringUtils.isNoneEmpty(encodedJwtBook)) {
//                book = JwtConfig.parseJwtConfigs(jwtSecretKey, encodedJwtBook, Book.class);
//            }
//            return Mono.justOrEmpty(book);
//        });
    }
}