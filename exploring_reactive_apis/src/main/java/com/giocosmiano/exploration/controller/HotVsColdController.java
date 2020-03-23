package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.HotVsColdEither;
import com.giocosmiano.exploration.service.HotVsColdObservableServiceScala;
import com.giocosmiano.exploration.service.HotVsColdReactiveService;
import io.reactivex.Single;
import io.reactivex.schedulers.Schedulers;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Optional;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

@RestController
@RequestMapping(value = "/primeNumbers")
public class HotVsColdController {

    private static final Logger log = LoggerFactory.getLogger(HotVsColdController.class);

    private HotVsColdReactiveService hotVsColdReactiveService;

    public HotVsColdController(
            HotVsColdReactiveService hotVsColdReactiveService
    ) {
        this.hotVsColdReactiveService = hotVsColdReactiveService;
    }

    @GetMapping(value = "/observables", produces = MediaType.APPLICATION_JSON_VALUE)
    public Single<ResponseEntity<List<HotVsColdEither>>> getObservablePrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using Observables, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService.getObservablePrimes(isHotObservable, threshold)
                .observeOn(Schedulers.computation()) // running on different thread
                .map(listOfPrimes -> ResponseEntity.ok(listOfPrimes))
                ;
    }

    @GetMapping(value = "/reactorFlux", produces = MediaType.APPLICATION_JSON_VALUE)
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

        return hotVsColdReactiveService.getFluxPrimes(isHotObservable, threshold)
                .subscribeOn(reactor.core.scheduler.Schedulers.elastic()) // running on different thread
                .map(listOfPrimes -> ResponseEntity.ok(listOfPrimes))
                ;
    }

    @GetMapping(value = "/scalaObservables", produces = MediaType.APPLICATION_JSON_VALUE)
    public rx.lang.scala.Observable<ResponseEntity<List<HotVsColdEither>>> getObservablePrimesFromScala(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Generating Prime Numbers, using Scala Observables, up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return new HotVsColdObservableServiceScala()
                .getObservablePrimesFromScala(isHotObservable, threshold)
                ;
    }
}
