package com.giocosmiano.exploration.aspect;

import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Signal;
import reactor.util.context.Context;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// https://www.youtube.com/watch?v=5tlZddM5Jo0 (Reactor Context)
// https://simonbasle.github.io/2018/02/contextual-logging-with-reactor-context-and-mdc/ (Contextual Logging with Reactor Context and MDC)
// https://ndportmann.com/passing-context-with-spring-webflux/ (Passing Context with Spring WebFlux Part 1)
// https://ndportmann.com/logging-with-context-in-spring-webflux/ (Logging with Context in Spring WebFlux Part 2)
// https://medium.com/@azizulhaq.ananto/how-to-handle-logs-and-tracing-in-spring-webflux-and-microservices-a0b45adc4610
// https://medium.com/asyncparadigm/logging-in-a-multithreaded-environment-and-with-completablefuture-construct-using-mdc-1c34c691cef0 (Logging in a multithreaded environment and with CompletableFuture construct using MDC)
@Log4j2
@Aspect
@Component
public class LoggingAspect {

  public static final String REQUEST_ID = "requestID";

  // https://www.youtube.com/watch?v=5tlZddM5Jo0 (Reactor Context)
  // https://simonbasle.github.io/2018/02/contextual-logging-with-reactor-context-and-mdc/ (Contextual Logging with Reactor Context and MDC)
  private static <T> Consumer<Signal<T>> logOnNext(Consumer<T> logStatement) {
    return signal -> {
      if (! signal.isOnNext()) return;

      // see logback-spring.xml for logging.pattern.console settings with added `requestID`
      // %black(%d{ISO8601}) %highlight(%-5level) [%blue(%t)] %yellow(%C{1.}): %cyan([%X{requestID}]) %msg%n%throwable
      Optional<String> requestID = signal.getContext().getOrEmpty(REQUEST_ID);

      if (requestID.isPresent()) {
        MDC.MDCCloseable closeable = MDC.putCloseable(REQUEST_ID, requestID.get());
      }
      logStatement.accept(signal.get());
    };
  }

  private Mono<String> loggingEntry(final ProceedingJoinPoint joinPoint) {
    return Mono
            .just(
                    String.format("Entering %s.%s with args %s"
                            , joinPoint.getSignature().getDeclaringType()
                            , joinPoint.getSignature().getName()
                            , Stream.of(joinPoint.getArgs())
                                    .filter(Objects::nonNull)
                                    .map(Object::toString)
                                    .collect(Collectors.joining(",", "[", "]"))
                    )
            );
  }

  @Around("execution(reactor.core.publisher.Mono+ com.giocosmiano.exploration.controller.*.*(..))")
  public Mono<?> loggingMonoWithAddedContext(final ProceedingJoinPoint joinPoint) {
    try {
      return loggingEntry(joinPoint)
              .doOnEach(logOnNext(log::info))
              .then((Mono<?>) joinPoint.proceed())
              .doOnEach(logOnNext(log::debug))
              .subscriberContext(Context.of(REQUEST_ID, UUID.randomUUID().toString()))
              ;
    } catch (Throwable throwable) {
      return Mono.error(throwable);
    }
  }

  @Around(
          "execution(reactor.core.publisher.Mono+ com.giocosmiano.exploration.service.*.*(..)) "
                  + " || execution(reactor.core.publisher.Mono+ com.giocosmiano.exploration.reactiveApis.*.*(..)) "
                  + " || execution(reactor.core.publisher.Mono+ com.giocosmiano.exploration.repository.*.*(..)) "
  )
  public Mono<?> loggingMono(final ProceedingJoinPoint joinPoint) {
    try {
      return loggingEntry(joinPoint)
              .doOnEach(logOnNext(log::info))
              .then((Mono<?>) joinPoint.proceed())
              .doOnEach(logOnNext(log::debug))
              ;
    } catch (Throwable throwable) {
      return Mono.error(throwable);
    }
  }

  @Around("execution(reactor.core.publisher.Flux+ com.giocosmiano.exploration.controller.*.*(..))")
  public Flux<?> loggingFluxWithAddedContext(final ProceedingJoinPoint joinPoint) {
    try {
      return loggingEntry(joinPoint)
              .doOnEach(logOnNext(log::info))
              .thenMany((Flux<?>) joinPoint.proceed())
              .doOnEach(logOnNext(log::debug))
              .subscriberContext(Context.of(REQUEST_ID, UUID.randomUUID().toString()))
              ;
    } catch (Throwable throwable) {
      return Flux.error(throwable);
    }
  }

  @Around(
          "execution(reactor.core.publisher.Flux+ com.giocosmiano.exploration.service.*.*(..)) "
                  + " || execution(reactor.core.publisher.Flux+ com.giocosmiano.exploration.reactiveApis.*.*(..)) "
                  + " || execution(reactor.core.publisher.Flux+ com.giocosmiano.exploration.repository.*.*(..)) "
  )
  public Flux<?> loggingFlux(final ProceedingJoinPoint joinPoint) {
    try {
      return loggingEntry(joinPoint)
              .doOnEach(logOnNext(log::info))
              .thenMany((Flux<?>) joinPoint.proceed())
              .doOnEach(logOnNext(log::debug))
              ;
    } catch (Throwable throwable) {
      return Flux.error(throwable);
    }
  }

  @Pointcut(
          "within(com.giocosmiano.exploration.service..*) "
                  + " || within(com.giocosmiano.exploration.reactiveApis..*) "
                  + " || within(com.giocosmiano.exploration.repository..*) "
  )
  public void applicationPointcut() {
    // empty body as it is a pointcut
  }

  @Pointcut(
          "within(com.giocosmiano.exploration.controller..*) "
  )
  public void controllerPointcut() {
    // empty body as it is a pointcut
  }
}
