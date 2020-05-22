package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.service.Jose4jService;
import com.giocosmiano.exploration.service.JwtService;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@Log4j2
@Controller
@RequestMapping(value = "/jwt")
public class JwtController {

    private final JwtService jwtService;
    private final Jose4jService jose4jService;

    public JwtController(JwtService jwtService, Jose4jService jose4jService) {
        this.jwtService = jwtService;
        this.jose4jService = jose4jService;
    }

    // Try this in Postman
    // secretKey ThisIsAVeryLongSecretKey1234$!!!
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

    // Try this in Postman
    // secretKey ThisIsAVeryLongSecretKey1234$!!!
    // eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI4Mjk1MzhhNC1lY2NjLTRhODgtYmMwNS0yN2RlYzZhNTM1ZTEiLCJzdWIiOiJzYW1wbGVTdWJqZWN0IiwiYXVkIjoic2FtcGxlQXVkaWVuY2UiLCJpc3MiOiJzYW1wbGVJc3N1ZXIiLCJpYXQiOjE1ODkwNjc0OTgsImV4cCI6MTU4OTE1Mzg5OCwiandzQm9keSI6IjdjUmpOK1QzanFEUkFqMWFKQzN2aEZVa1VoejBsWGZhTzFxYUM4NG1hdnFPcC91YTREblJwSEdFZjFJT09RdnZHUmNhSnQrWnFyYVlmK2JkVUNCVnVQdEpIb25XR2F2Qy9vNzlmVmJSK2lNK1pXV2xVTU14SjBrcXQxQ1J4WFZQS1MrRU16U2pYSXBQR3VPcXhONFdmVUJxSUNkNW5EVllvNlVDYXg0R1RpYlRKQzg1K2pCMmJHWnE1LzdQL0RhUllXcWlwT1BvYW4wWDdvOHhYRG5vQm9jSWNHdkNBczNJUTNkZmRVbHZKeHVqMnMxZndDODRleWI2Zm16OTlLNHU3UkpWT0xIZ0xGRXZJMjdPV2tIR24ySWhvUkpqZDh1Wi9scExQQk91WCtSd3k1SGN0MFpkdktRcTk4SUJ6ZGtOeFpwMEhDZS9TTy94TjlSdHk0WkJTZ0dMTXcwYkRROVVLeEJkaU4rTWhVb1VCSlExMEdnS2YvQkE0M0dTbUtGZTE1b0dEbitpejdteUhEMVB4TUtGS2g5NG5yNkxJL0E0TEhEd3RNcU9NTTNWY2lHUU9xWGxia1pqZ2o3c0xlb2hWWHM3TjZxQk43N0JZMkJ5ZnpoRlEzUSs4KzhlazY5YmZaTGNiL04vWE9FPSJ9.MANRS-cr-fyPPBWnFPOTY2axwrMaqcPuBkOPf49lMVk
    @ResponseBody
    @PostMapping(value = "parseJwt", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> parseJwtConfigs(@RequestBody final String encodedJwtBook) {
        return jwtService
                .parseJwtConfigs(encodedJwtBook)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    // Try this in Postman
    // secretKey ThisIsAVeryLongSecretKey1234$!!!
    // { "_id" : 265, "title" : "Scala in Action", "isbn" : "1935182757", "pageCount" : 0, "publishedDate" : "2013-04-09T00:00:00.000-0700", "thumbnailUrl" : "https://s3.amazonaws.com/AKIAJC5RLADLUMVRPFDQ.book-thumb-images/raychaudhuri.jpg", "status" : "PUBLISH", "authors" : [ "Nilanjan Raychaudhuri" ], "categories" : [ "Java" ] }
    @ResponseBody
    @PostMapping(value = "generateJwe", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<String>> generateJweConfigs(@RequestBody final Book book) {
        return jose4jService
                .generateJweConfigs(book)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }

    // Try this in Postman
    // secretKey ThisIsAVeryLongSecretKey1234$!!!
    // eyJhbGciOiJBMTI4S1ciLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0.aD2Mf5O4WHCgdc5RVi1Y9Pp3pxCNYlNiCczWVV2M-6PLCon0p3HKEg.zFn7itG_Yp-HpQUL4TvMOg.itPReh3qwPax42WPjXzqp_TQRiM85spO0q8ZNi-e9_VkLpJ36JP4bKx5mJneXA1-ihMlRqk4ePgxhHRU_kVbyn3e_c3KMSfXK_CH48Jfkt-1GM4vS3P8ofAfK1W8XWs8p0TOZz1drx5PwAVmJzQqqC8qtXnFVCOJkOZZP41GI-uw5JGLrhQv8NZNq00ulZ0sQ1pZBzPUtAGmapPdf4hGcNXv7yggVhph1OuAhkBrvUXtBkRI3I85cLkmMekdfJRIOWtleSHljgpQTsorxEwJ8Nt_JorgmASXCNSjufr122_QTaBvBCOKLFXG75ZW6_mwzA9FNpr0z_UzhEnwVASAgDMKsE8LsQ3TQOf5rnPB337DLsizp6QR-8jOtsaEHbHsArVli_rbiXthJQes2MP5UqvlPi4sih7lDynEip4bkRrxS_OFnsOY00tZLW4DlAnl.w61l3WI5awV6WM7EWex4Ng
    @ResponseBody
    @PostMapping(value = "parseJwe", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<Book>> parseJweConfigs(@RequestBody final String encodedJweBook) {
        return jose4jService
                .parseJweConfigs(encodedJweBook)
                .map(ResponseEntity::ok)
                .switchIfEmpty(Mono.just(ResponseEntity.badRequest().build()))
                ;
    }
}
