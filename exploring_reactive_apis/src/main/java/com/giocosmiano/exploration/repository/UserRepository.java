package com.giocosmiano.exploration.repository;

import com.giocosmiano.exploration.domain.User;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;

public interface UserRepository
        extends CrudRepository<User, Long> {

    Optional<User> findByUsername(String username);

}