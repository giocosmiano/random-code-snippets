package com.giocosmiano.exploration.repository;

import com.giocosmiano.exploration.domain.H2User;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;

public interface H2UserRepository
        extends CrudRepository<H2User, Long> {

    Optional<H2User> findByUsername(String username);

}