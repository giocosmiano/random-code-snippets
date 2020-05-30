package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.config.Pbkdf2PasswordEncoderConfig;
import com.giocosmiano.exploration.domain.User;
import com.giocosmiano.exploration.repository.UserRepository;
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
public class UserService {

    private final Scheduler jdbcScheduler;
    private final TransactionTemplate transactionTemplate;
    private final UserRepository userRepository;
    private final Pbkdf2PasswordEncoderConfig pbkdf2PasswordEncoderConfig;

    public UserService(
            @Qualifier("jdbcScheduler") Scheduler jdbcScheduler
            , TransactionTemplate transactionTemplate
            , UserRepository userRepository
            , Pbkdf2PasswordEncoderConfig pbkdf2PasswordEncoderConfig
    ) {
        this.jdbcScheduler = jdbcScheduler;
        this.transactionTemplate = transactionTemplate;
        this.userRepository = userRepository;
        this.pbkdf2PasswordEncoderConfig = pbkdf2PasswordEncoderConfig;
    }

    public Mono<User> getById(final Long id) {
        return Mono
                .defer(() -> Mono.justOrEmpty(userRepository.findById(id))) // using `defer` to re-evaluate the lambda for each request thus making lazy IO call
                .subscribeOn(jdbcScheduler) // while running the request on different thread-pool
                ;
    }

    public Flux<User> getAllUsers() {
        return Flux
                .defer(() -> Flux.fromIterable(userRepository.findAll())) // using `defer` to re-evaluate the lambda for each request thus making lazy IO call
                .subscribeOn(jdbcScheduler) // while running the request on different thread-pool
                ;
    }

    public Mono<User> create(final User user) {
        if (Objects.nonNull(user)) {
            user.setId(null);
            user.setActive(true);
            user.setPassword(pbkdf2PasswordEncoderConfig.getPbkdf2PasswordEncoder().encode(user.getPassword()));
            return Mono
                    .fromCallable(() -> transactionTemplate.execute(status -> userRepository.save(user)))
                    .log("userService.create() on log()" + user)
                    .subscribeOn(jdbcScheduler) // running the request on different thread-pool
            ;

        } else {
            return Mono.empty();
        }
    }

    public Mono<User> update(final User user) {
        if (Objects.nonNull(user) && Objects.nonNull(user.getId())) {
            return Mono
                    .fromCallable(() ->
                            transactionTemplate.execute(status -> {
                                User oldUser = userRepository.findById(user.getId()).orElse(null);
                                if (Objects.isNull(oldUser)) {
                                    return oldUser;
                                }
                                if (! user.getPassword().equals(oldUser.getPassword())) {
                                    user.setPassword(pbkdf2PasswordEncoderConfig.getPbkdf2PasswordEncoder().encode(user.getPassword()));
                                }
                                return userRepository.save(user);
                            })
                    )
                    .log("userService.update() on log()" + user)
                    .subscribeOn(jdbcScheduler) // running the request on different thread-pool
                    ;

        } else {
            return Mono.empty();
        }
    }

    // http://zetcode.com/all/#springboot
    // http://zetcode.com/springboot/mongodbreactive/
    // https://mkyong.com/mongodb/spring-data-mongodb-update-document/
    // https://medium.com/@nikeshshetty/5-common-mistakes-of-webflux-novices-f8eda0cd6291
    // https://www.devglan.com/spring-boot/spring-boot-mongodb-crud
    // https://www.roytuts.com/spring-boot-mongodb-functional-reactive-crud-example/
    public Mono<User> delete(final Long id) {
        return Mono
                .fromCallable(() ->
                        transactionTemplate.execute(status -> {
                            User oldUser = userRepository.findById(id).orElse(null);
                            if (Objects.isNull(oldUser)) {
                                return oldUser;
                            }
                            userRepository.delete(oldUser);
                            return oldUser;
                        })
                )
                .log("userService.delete() on log()" + id)
                .subscribeOn(jdbcScheduler) // running the request on different thread-pool
                ;
    }
}