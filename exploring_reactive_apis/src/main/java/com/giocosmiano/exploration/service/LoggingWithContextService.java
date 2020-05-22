package com.giocosmiano.exploration.service;

import lombok.extern.log4j.Log4j2;
import org.slf4j.MDC;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Signal;
import reactor.util.context.Context;
import reactor.util.function.Tuple2;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

import static com.giocosmiano.exploration.aspect.LoggingAspect.*;

@Log4j2
public abstract class LoggingWithContextService {

    // https://www.youtube.com/watch?v=5tlZddM5Jo0 (Reactor Context)
    // https://simonbasle.github.io/2018/02/contextual-logging-with-reactor-context-and-mdc/ (Contextual Logging with Reactor Context and MDC)
    protected <T> Consumer<Signal<T>> logOnNext(Consumer<T> logStatement) {
        return signal -> {
            if (! signal.isOnNext()) return;

            // see logback-spring.xml for logging.pattern.console settings with added `requestID`
            // %black(%d{ISO8601}) %highlight(%-5level) [%blue(%t)] %yellow(%C{1.}): %cyan([%X{requestID}]) %msg%n%throwable
            Optional<String> requestID = signal.getContext().getOrEmpty(REQUEST_ID);

            if (requestID.isPresent()) {
                try (MDC.MDCCloseable closeable = MDC.putCloseable(REQUEST_ID, requestID.get())) {
                    logStatement.accept(signal.get());
                }

            } else {
                logStatement.accept(signal.get());
            }
        };
    }

    // Exploring: Thread-local state availability in reactive services
    // https://kamilszymanski.github.io/thread-local-state-availability-in-reactive-services/
    // https://dzone.com/articles/thread-local-state-availability-in-reactive-servic
    protected <T, T2> Mono<T> loggingMonoOnNextWithContext(final Tuple2<T, T2> tuple) {
        return Mono.fromCallable(() -> {
            T  data = tuple.getT1();
            T2 ctx  = tuple.getT2();
            try (MDC.MDCCloseable closeable = MDC.putCloseable(REQUEST_ID, ((Context)ctx).get(REQUEST_ID))) {
                log.info("Thread '{}', class '{}', data '{}'", Thread.currentThread().getName(), this.getClass().getName(), data);
            }
            return data;
        });
    }

    // Exploring: Thread-local state availability in reactive services
    // https://kamilszymanski.github.io/thread-local-state-availability-in-reactive-services/
    // https://dzone.com/articles/thread-local-state-availability-in-reactive-servic
    protected <T, T2> Function<Tuple2<T, T2>, Mono<T>> flatMapMonoToLogWithContext(Function<Tuple2<T, T2>, Mono<T>> mapper) {
        return mapper; // OR
//        return mapper::apply; // OR
//        return dataAndCtxTuple -> {
//            T  data = dataAndCtxTuple.getT1();
//            T2 ctx  = dataAndCtxTuple.getT2();
//            try (MDC.MDCCloseable mdc = MDC.putCloseable(REQUEST_ID, ((Context)ctx).get(REQUEST_ID))) {
//                log.info(" Thread " + Thread.currentThread().getName() + " data ==> " + data);
//            }
//            return mapper.apply(dataAndCtxTuple);
//        };
    }
}