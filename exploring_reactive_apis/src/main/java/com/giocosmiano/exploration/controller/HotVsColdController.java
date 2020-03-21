package com.giocosmiano.exploration.controller;

import com.giocosmiano.exploration.reactiveApis.HotVsColdObservables;
import com.giocosmiano.exploration.reactiveApis.HotVsColdReactorFlux;
import io.reactivex.Observable;
import io.vavr.control.Either;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static com.giocosmiano.exploration.reactiveApis.HotVsColdUtilities.*;

@RestController
public class HotVsColdController {

    private static final Logger log = LoggerFactory.getLogger(HotVsColdController.class);

    private HotVsColdObservables hotVsColdObservables;
    private HotVsColdReactorFlux hotVsColdReactorFlux;

    public HotVsColdController(
            HotVsColdObservables hotVsColdObservables
            , HotVsColdReactorFlux hotVsColdReactorFlux
    ) {
        this.hotVsColdObservables = hotVsColdObservables;
        this.hotVsColdReactorFlux = hotVsColdReactorFlux;
    }

    @GetMapping("/observable/primes")
    public Observable<CompletableFuture<Either<String,Integer>>> getObservablePrimes(@RequestParam("type") String type, @RequestParam("threshold") Integer requestedThreshold) {
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        return hotVsColdObservables.runObservable(StringUtils.equalsIgnoreCase(type, "hot"), threshold);
    }

    @GetMapping("/flux/primes")
    public Flux<CompletableFuture<Either<String,Integer>>> getFluxPrimes(@RequestParam("type") String type, @RequestParam("threshold") Integer requestedThreshold) {
        Integer threshold = Optional.ofNullable(requestedThreshold).orElseGet(() -> DEFAULT_THRESHOLD);
        return hotVsColdReactorFlux.runObservable(StringUtils.equalsIgnoreCase(type, "hot"), threshold);
    }
}
