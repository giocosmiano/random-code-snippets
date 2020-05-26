package com.giocosmiano.exploration.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.filter.CommonsRequestLoggingFilter;

@Configuration
public class RequestLoggingFilterConfig {

    @Bean
    public CommonsRequestLoggingFilter logFilter() {
        CommonsRequestLoggingFilter filter
                = new CommonsRequestLoggingFilter();
        filter.setIncludeQueryString(true);
        filter.setIncludePayload(true);
        filter.setMaxPayloadLength(128000);
//        filter.setIncludeHeaders(false);
//        filter.setIncludeClientInfo(true);
//        filter.setAfterMessagePrefix("REQUEST DATA : ");
        return filter;
    }
}