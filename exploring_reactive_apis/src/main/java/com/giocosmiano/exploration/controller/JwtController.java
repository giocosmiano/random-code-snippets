package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.service.JwtService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@Controller
@RequestMapping(value = "/jwt")
@CrossOrigin(origins = "*", allowedHeaders = "*")
public class JwtController {

    private final JwtService jwtService;

    public JwtController(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    private static final Logger log = LoggerFactory.getLogger(JwtController.class);

    // try this in https://jwt.io/ or postman
    // secretKey Th1s1sAV3ryL0ngSecretKeyIndeed!!$
    // { "_id" : 265, "title" : "Scala in Action", "isbn" : "1935182757", "pageCount" : 0, "publishedDate" : "2013-04-09T00:00:00.000-0700", "thumbnailUrl" : "https://s3.amazonaws.com/AKIAJC5RLADLUMVRPFDQ.book-thumb-images/raychaudhuri.jpg", "status" : "PUBLISH", "authors" : [ "Nilanjan Raychaudhuri" ], "categories" : [ "Java" ] }
    @ResponseBody
    @PostMapping(value = "generateJwt", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<String>> generateJwtConfigs(@RequestBody final Book book) {
        return jwtService
                .generateJwtConfigs(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    // try this in https://jwt.io/ or postman
    // secretKey Th1s1sAV3ryL0ngSecretKeyIndeed!!$
    // eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI5NTRkMzkzMS1mZGNmLTRmZDQtOGM0Ny01ZjVmMzdiN2Q2YjAiLCJzdWIiOiJ1c2VySWRfMTIzIiwiYXVkIjoiYXVkaWVuY2UiLCJpc3MiOiJzYW1wbGVJc3N1ZXIiLCJpYXQiOjE1ODY1MzMwODUsImV4cCI6MTU4NjYxOTQ4NSwiandzQm9keSI6IntcImlkXCI6bnVsbCxcInRpdGxlXCI6XCJTY2FsYSBpbiBBY3Rpb25cIixcImlzYm5cIjpcIjE5MzUxODI3NTdcIixcInBhZ2VDb3VudFwiOjAsXCJwdWJsaXNoZWREYXRlXCI6MTM2NTQ5MDgwMDAwMCxcInRodW1ibmFpbFVybFwiOlwiaHR0cHM6Ly9zMy5hbWF6b25hd3MuY29tL0FLSUFKQzVSTEFETFVNVlJQRkRRLmJvb2stdGh1bWItaW1hZ2VzL3JheWNoYXVkaHVyaS5qcGdcIixcInNob3J0RGVzY3JpcHRpb25cIjpudWxsLFwibG9uZ0Rlc2NyaXB0aW9uXCI6bnVsbCxcInN0YXR1c1wiOlwiUFVCTElTSFwiLFwiYXV0aG9yc1wiOltcIk5pbGFuamFuIFJheWNoYXVkaHVyaVwiXSxcImNhdGVnb3JpZXNcIjpbXCJKYXZhXCJdfSJ9.4WOk_heJduWYjhI2yPNZatPGhfQDDh2dUfk6CMvo5fk
    @ResponseBody
    @PostMapping(value = "parseJwt", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> parseJwtConfigs(@RequestBody final String encodedJwtBook) {
        return jwtService
                .parseJwtConfigs(encodedJwtBook)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }
}
