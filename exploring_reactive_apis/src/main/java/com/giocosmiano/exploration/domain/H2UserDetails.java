package com.giocosmiano.exploration.domain;

import lombok.extern.log4j.Log4j2;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Log4j2
public class H2UserDetails implements UserDetails {

    private static final long serialVersionUID = -4617834194137726757L;

    private final String username;
    private final String password;
    private final boolean active;
    private final List<GrantedAuthority> authorities;

    public H2UserDetails(final H2User h2User) {
        this.username = h2User.getUsername();
        this.password = h2User.getPassword();
        this.active = h2User.isActive();
        this.authorities = Arrays.stream(h2User.getRoles())
                    .map(SimpleGrantedAuthority::new)
                    .collect(Collectors.toList());
    }

    @Override
    public String getUsername() {
        return username;
    }

    @Override
    public String getPassword() {
        return password;
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return active;
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return authorities;
    }
}
