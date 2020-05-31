package com.giocosmiano.exploration.config;

import com.giocosmiano.exploration.service.CustomUserDetailsService;
import lombok.extern.log4j.Log4j2;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.crypto.password.PasswordEncoder;

@Log4j2
@Configuration
@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {

    private final CustomUserDetailsService customUserDetailsService;
    private final Pbkdf2PasswordEncoderConfig pbkdf2PasswordEncoderConfig;

    public SecurityConfiguration(
            CustomUserDetailsService customUserDetailsService
            , Pbkdf2PasswordEncoderConfig pbkdf2PasswordEncoderConfig
    ) {
        this.customUserDetailsService = customUserDetailsService;
        this.pbkdf2PasswordEncoderConfig = pbkdf2PasswordEncoderConfig;
    }

    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(customUserDetailsService)
            .passwordEncoder(getPasswordEncoder());
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {

        // TODO: use for local development and debugging service/component purposes,
        //  ignoring all security checks for all requests
/*
        http.csrf().disable().headers().frameOptions().sameOrigin()
            .and().authorizeRequests()
            .anyRequest().permitAll()
            .and().httpBasic().disable()
        ;
*/

        http.authorizeRequests()
            .antMatchers("/static/**").permitAll()
            .antMatchers("/templates/**").permitAll()
//                .antMatchers("/h2-console/**").permitAll()
            .antMatchers("/admin").hasRole("ADMIN")
//                .antMatchers("/users").hasAnyRole("ADMIN", "USER")
            .antMatchers("/").permitAll()
            .anyRequest().authenticated()
            .and().formLogin().permitAll()
//                .and().formLogin().loginPage("/login").permitAll()
            .and().logout().permitAll()
        ;

        // to enable /h2-console from spring security
//        http.csrf().disable();
//        http.headers().frameOptions().disable();
    }

    @Bean
    public PasswordEncoder getPasswordEncoder() {
        return pbkdf2PasswordEncoderConfig.getPbkdf2PasswordEncoder();
    }
}