package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.domain.HotVsColdEither;
import com.giocosmiano.exploration.service.HotVsColdReactiveService;
import io.reactivex.Single;
import io.reactivex.schedulers.Schedulers;
import io.vavr.control.Either;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

@RestController
public class HotVsColdController {

    private static final Logger log = LoggerFactory.getLogger(HotVsColdController.class);

    private HotVsColdReactiveService hotVsColdReactiveService;

    public HotVsColdController(
            HotVsColdReactiveService hotVsColdReactiveService
    ) {
        this.hotVsColdReactiveService = hotVsColdReactiveService;
    }

    @GetMapping(value = "/observable/primes", produces = MediaType.APPLICATION_JSON_VALUE)
    public Single<ResponseEntity<List<HotVsColdEither>>> getObservablePrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Requested %s Observable Prime Numbers up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService.getObservablePrimes(isHotObservable, threshold)
                .subscribeOn(Schedulers.computation()) // running on different thread
                .map(listOfPrimes -> ResponseEntity.ok(listOfPrimes));
    }

    @GetMapping(value = "/flux/primes", produces = MediaType.APPLICATION_JSON_VALUE)
    public Flux<CompletableFuture<Either<String,Integer>>> getFluxPrimes(
            @RequestParam(name = "type", required = false) String type
            , @RequestParam(name = "threshold", required = false) Integer requestedThreshold
    ) {
        boolean isHotObservable = StringUtils.equalsIgnoreCase(type, "hot");
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        log.info(
                String.format("Requested %s Reactor Flux Prime Numbers up-to threshold limit=%s (defaulting to %s, if not provided) from %s"
                        , isHotObservable ? "Hot" : "Cold"
                        , requestedThreshold
                        , DEFAULT_THRESHOLD
                        , Thread.currentThread().getName()
                ));

        return hotVsColdReactiveService.getFluxPrimes(isHotObservable, threshold);
    }
}
