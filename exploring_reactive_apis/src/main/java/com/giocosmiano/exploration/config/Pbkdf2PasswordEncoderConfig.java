package com.giocosmiano.exploration.config;

import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.password.Pbkdf2PasswordEncoder;

@Log4j2
@Configuration
public class Pbkdf2PasswordEncoderConfig {

    @Value("${pwd-encoder.secret-key}")
    private String secretKey;

    @Value("${pwd-encoder.iteration}")
    private int iteration;

    @Value("${pwd-encoder.hash-width}")
    private int hashWidth;

    @Bean
    public Pbkdf2PasswordEncoder getPbkdf2PasswordEncoder() {
        return new Pbkdf2PasswordEncoder(secretKey, iteration, hashWidth);
    }
}