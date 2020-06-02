package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.H2User;
import com.giocosmiano.exploration.domain.H2UserDetails;
import com.giocosmiano.exploration.repository.H2UserRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Log4j2
@Service
public class H2UserDetailsService implements UserDetailsService {

    private final H2UserRepository h2UserRepository;

    public H2UserDetailsService(H2UserRepository h2UserRepository) {
        this.h2UserRepository = h2UserRepository;
    }

    @Override
    public UserDetails loadUserByUsername(final String username) throws UsernameNotFoundException {
        Optional<H2User> user =
                h2UserRepository.findByUsername(username);

        if (! user.isPresent()) {
            throw new UsernameNotFoundException(username);
        }

        return new H2UserDetails(user.get());
    }
}