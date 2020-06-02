package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.config.Pbkdf2PasswordEncoderConfig;
import com.giocosmiano.exploration.domain.H2User;
import com.giocosmiano.exploration.repository.H2UserRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

import java.util.Objects;

@Log4j2
@Service
public class H2UserService {

    private final Scheduler jdbcScheduler;
    private final TransactionTemplate transactionTemplate;
    private final H2UserRepository h2UserRepository;
    private final Pbkdf2PasswordEncoderConfig pbkdf2PasswordEncoderConfig;

    public H2UserService(
            @Qualifier("jdbcScheduler") Scheduler jdbcScheduler
            , TransactionTemplate transactionTemplate
            , H2UserRepository h2UserRepository
            , Pbkdf2PasswordEncoderConfig pbkdf2PasswordEncoderConfig
    ) {
        this.jdbcScheduler = jdbcScheduler;
        this.transactionTemplate = transactionTemplate;
        this.h2UserRepository = h2UserRepository;
        this.pbkdf2PasswordEncoderConfig = pbkdf2PasswordEncoderConfig;
    }

    public Mono<H2User> getById(final Long id) {
        return Mono
                .defer(() -> Mono.justOrEmpty(h2UserRepository.findById(id)))
                .subscribeOn(jdbcScheduler)
                ;
    }

    public Mono<H2User> getByUsername(final String userName) {
        return Mono
                .defer(() -> Mono.justOrEmpty(h2UserRepository.findByUsername(userName)))
                .subscribeOn(jdbcScheduler)
                ;
    }

    public Mono<Long> getCounts() {
        return Mono
                .defer(() -> Mono.just(h2UserRepository.count()))
                .subscribeOn(jdbcScheduler)
                ;
    }

    public Flux<H2User> getAllUsers() {
        return Flux
                .defer(() -> Flux.fromIterable(h2UserRepository.findAll()))
                .subscribeOn(jdbcScheduler)
                ;
    }

    public Mono<H2User> create(final H2User h2User) {
        if (Objects.nonNull(h2User)) {
            h2User.setId(null);
            h2User.setActive(true);
            h2User.setPassword(pbkdf2PasswordEncoderConfig.getPbkdf2PasswordEncoder().encode(h2User.getPassword()));
            return Mono
                    .fromCallable(() -> transactionTemplate.execute(status -> h2UserRepository.save(h2User)))
                    .log("userService.create() on log()" + h2User)
                    .subscribeOn(jdbcScheduler)
            ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<H2User> update(final H2User h2User) {
        if (Objects.nonNull(h2User) && Objects.nonNull(h2User.getId())) {
            return Mono
                    .fromCallable(() ->
                            transactionTemplate.execute(status -> {
                                H2User oldH2User = h2UserRepository.findById(h2User.getId()).orElse(null);
                                if (Objects.isNull(oldH2User)) {
                                    return oldH2User;
                                }
                                if (! h2User.getPassword().equals(oldH2User.getPassword())) {
                                    h2User.setPassword(pbkdf2PasswordEncoderConfig.getPbkdf2PasswordEncoder().encode(h2User.getPassword()));
                                }
                                return h2UserRepository.save(h2User);
                            })
                    )
                    .log("userService.update() on log()" + h2User)
                    .subscribeOn(jdbcScheduler)
                    ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<H2User> delete(final Long id) {
        return Mono
                .fromCallable(() ->
                        transactionTemplate.execute(status -> {
                            H2User oldH2User = h2UserRepository.findById(id).orElse(null);
                            if (Objects.isNull(oldH2User)) {
                                return oldH2User;
                            }
                            h2UserRepository.delete(oldH2User);
                            return oldH2User;
                        })
                )
                .log("userService.delete() on log()" + id)
                .subscribeOn(jdbcScheduler)
                ;
    }
}