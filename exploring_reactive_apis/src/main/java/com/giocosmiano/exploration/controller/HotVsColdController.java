package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.HotVsColdEither;
import com.giocosmiano.exploration.service.HotVsColdObservableServiceScala;
import com.giocosmiano.exploration.reactiveApis.HotVsColdObservablesGroovy;
import com.giocosmiano.exploration.service.HotVsColdReactiveService;
import io.reactivex.Observable;
import io.reactivex.Single;
import io.reactivex.schedulers.Schedulers;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Optional;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

@Log4j2
@Controller
@RequestMapping(value = "/hotVsColdObservables")
public class HotVsColdController {

    private final HotVsColdReactiveService hotVsColdReactiveService;

    public HotVsColdController(
            HotVsColdReactiveService hotVsColdReactiveService
    ) {
        this.hotVsColdReactiveService = hotVsColdReactiveService;
    }

    @GetMapping
    public Mono<String> index(Model model) {
        model.addAttribute("restEndpoint", "/hotVsColdObservables/primeStreamReactorFlux");
        model.addAttribute("pageTitle", "Streaming Simulations using RxJS and Oboe.js in the UI while Reactor Flux on the back-end");
        return Mono.just("hotVsColdObservables");
    }

    @GetMapping(value = "/streamingObservables")
    public Mono<String> streamingObservables(Model model) {
        model.addAttribute("restEndpoint", "/hotVsColdObservables/primeStreamObservables");
        model.addAttribute("pageTitle", "Streaming Simulations using RxJS and Oboe.js in the UI while RxJava on the back-end");
        return Mono.just("hotVsColdObservables");
    }

    @GetMapping(value = "/view")
    public Mono<String> standAlone() {
        return Mono.just("hotVsColdObservablesSA");
    }

    // NOTE: Returning Single<ResponseEntity> worked because the collection is wrapped within ResponseEntity
    // Another implementation of getObservablePrimesOLD() below getting around Http response 503
    @GetMapping(value = "/primeObservables", produces = MediaType.APPLICATION_JSON_VALUE)
    public Single<ResponseEntity<List<HotVsColdEither>>> getObservablePrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using RxJava Observables, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService
                .getObservablePrimes(isHotObservable, threshold)
                .map(ResponseEntity::ok)
//                .subscribeOn(Schedulers.computation()) // testing a use case of running on different thread
                ;
    }

    // NOTE: Don't use ResponseEntity when returning Observable because it'll include headers, body (with the data), statusCode and statusCodeValue
    // Another implementation of getObservablePrimes() above that's causing Http 503 due to CompletableFuture within Observable
    // org.springframework.web.context.request.async.AsyncRequestTimeoutException
    // https://stackoverflow.com/questions/39856198/recurring-asyncrequesttimeoutexception-in-spring-boot-admin-log
    // https://stackoverflow.com/questions/53650303/spring-boot-timeout-when-using-futures/53652640
    @Deprecated
    @GetMapping(value = "/primeObservablesOLD", produces = MediaType.APPLICATION_JSON_VALUE)
    public Observable<HotVsColdEither> getObservablePrimesOLD(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using RxJava Observables, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService
                .getObservablePrimesOLD(isHotObservable, threshold)
//                .subscribeOn(Schedulers.computation()) // testing a use case of running on different thread
                ;
    }

    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see hotVsColdObservables.html)
    @GetMapping(value = "/primeStreamObservables", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Observable<ResponseEntity<HotVsColdEither>> getStreamObservablePrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using RxJava Observable Streams, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService
                .getStreamObservablePrimes(isHotObservable, threshold)
                .map(ResponseEntity::ok)
//                .subscribeOn(Schedulers.computation()) // testing a use case of running on different thread
                ;
    }

    // NOTE: Returning Mono<ResponseEntity> worked because the collection is wrapped within ResponseEntity
    @GetMapping(value = "/primeReactorFlux", produces = MediaType.APPLICATION_JSON_VALUE)
    public Mono<ResponseEntity<List<HotVsColdEither>>> getFluxPrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using Reactor Flux, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService
                .getFluxPrimes(isHotObservable, threshold)
                .map(ResponseEntity::ok)
//                .subscribeOn(reactor.core.scheduler.Schedulers.elastic()) // testing a use case of running on different thread
                ;
    }

    // Wrapping each element with ResponseEntity so that Oboe.js can watch/parse for `body` element in the UI (see hotVsColdObservables.html)
    @GetMapping(value = "/primeStreamReactorFlux", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Flux<ResponseEntity<HotVsColdEither>> getStreamFluxPrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using Reactor Flux Streams, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService
                .getStreamFluxPrimes(isHotObservable, threshold)
                .map(ResponseEntity::ok)
//                .subscribeOn(reactor.core.scheduler.Schedulers.elastic()) // testing a use case of running on different thread
                ;
    }

    // NOTE: Returning Single<ResponseEntity> worked because the collection is wrapped within ResponseEntity
    @GetMapping(value = "/primeGroovyObservables", produces = MediaType.APPLICATION_JSON_VALUE)
    public Single<ResponseEntity<List<HotVsColdEither>>> getObservablePrimesFromGroovy(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using RxGroovy Observables, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return new HotVsColdObservablesGroovy()
                .runObservable(isHotObservable, threshold)
                .map(ResponseEntity::ok)
                .subscribeOn(Schedulers.computation()) // testing a use case of running on different thread
                ;
    }

    // NOTE: Don't use ResponseEntity when returning Observable because it'll include headers, body (with the data), statusCode and statusCodeValue
    @GetMapping(value = "/primeStreamGroovyObservables", produces = MediaType.APPLICATION_STREAM_JSON_VALUE)
    public Observable<HotVsColdEither> getStreamObservablePrimesFromGroovy(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using RxGroovy Observable Streams, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return new HotVsColdObservablesGroovy()
                .runStreamObservable(isHotObservable, threshold)
//                .subscribeOn(Schedulers.computation()) // testing a use case of running on different thread
                ;
    }

    // NOTE: RxScala has been EOL
    // https://github.com/ReactiveX/RxScala
    // https://github.com/ReactiveX/RxScala/issues/244
    @GetMapping(value = "/primeScalaObservables", produces = MediaType.APPLICATION_JSON_VALUE)
    public rx.lang.scala.Observable<List<HotVsColdEither>> getObservablePrimesFromScala(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using RxScala Observables, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        // NOTE: RxScala has been EOL
        // https://github.com/ReactiveX/RxScala
        // https://github.com/ReactiveX/RxScala/issues/244
        return new HotVsColdObservableServiceScala()
                .getObservablePrimesFromScala(isHotObservable, threshold)
                .subscribeOn(rx.lang.scala.schedulers.ComputationScheduler.apply()) // running on different thread
                ;
    }
}
