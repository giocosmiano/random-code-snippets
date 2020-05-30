package com.giocosmiano.exploration.service;

import com.giocosmiano.exploration.domain.CustomUserDetails;
import com.giocosmiano.exploration.domain.User;
import com.giocosmiano.exploration.repository.UserRepository;
import lombok.extern.log4j.Log4j2;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Log4j2
@Service
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    public CustomUserDetailsService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public UserDetails loadUserByUsername(final String username) throws UsernameNotFoundException {
        Optional<User> user =
                userRepository.findByUsername(username);

        if (! user.isPresent()) {
            throw new UsernameNotFoundException(username);
        }

        return new CustomUserDetails(user.get());
    }
}