package com.giocosmiano.exploration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.web.filter.reactive.HiddenHttpMethodFilter;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import java.util.concurrent.Executors;

@SpringBootApplication
public class ExploringReactiveApplication {

	@Value("${spring.datasource.hikari.maximum-pool-size}")
	private int connectionPoolSize;

	public static void main(String[] args) {
		SpringApplication.run(ExploringReactiveApplication.class, args);
	}

	// NOTE: These jdbcScheduler and transactionTemplate are beans to support async JDBC calls
	// Using H2 blocking DB to simulate access with Reactor Flux/Mono to be non-blocking IO
	// See the following for references
	// https://jira.spring.io/browse/DATACMNS-1413 - Reactive Repositories are not supported by JPA. Need spring-webflux + spring-data-jpa combination get working
	// https://stackoverflow.com/questions/52480271/spring-webflux-jpa-reactive-repositories-are-not-supported-by-jpa
	// https://github.com/chang-chao/spring-webflux-async-jdbc-sample
	// https://github.com/davidmoten/rxjava2-jdbc
	// https://medium.com/@Baimurzin/reactive-spring-with-sql-databases-guide-using-spring-data-repositories-f55bf05e6b3
	@Bean
	public Scheduler jdbcScheduler() {
		return Schedulers.fromExecutor(Executors.newFixedThreadPool(connectionPoolSize));
	}

	@Bean
	public TransactionTemplate transactionTemplate(PlatformTransactionManager transactionManager) {
		return new TransactionTemplate(transactionManager);
	}

	@Bean
	HiddenHttpMethodFilter hiddenHttpMethodFilter() {
		return new HiddenHttpMethodFilter();
	}

	/*
	 HiddenHttpMethodFilter Spring bean to make the HTTP DELETE methods work properly
	 DELETE is not a valid action for an HTML5 FORM, so Thymeleaf creates a hidden input field
	 containing our desired verb while the enclosing form uses an HTML5 POST . This gets
	 transformed by Spring during the web call, resulting in the @DeleteMapping method being
	 properly invoked with no effort on our end.
	 */
}
