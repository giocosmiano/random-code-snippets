package com.giocosmiano.exploration.controller;

import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import reactor.core.publisher.Mono;

@Log4j2
@Controller
@RequestMapping(value = "/")
public class HomeController {

    @GetMapping
    public Mono<String> index(Model model) {
        model.addAttribute("restEndpoint", "/books/streaming");
        model.addAttribute("repository", "mongo");
        model.addAttribute("pageTitle", "Streaming Simulation using RxJS and Oboe.js in the UI while Reactor Flux, with Reactive Mongo DB-driver, on the back-end");
        return Mono.just("booksReactorFlux");
    }
}
