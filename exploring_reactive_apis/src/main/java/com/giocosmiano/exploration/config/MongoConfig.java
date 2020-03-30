package com.giocosmiano.exploration.config;

import com.mongodb.reactivestreams.client.MongoClient;
import com.mongodb.reactivestreams.client.MongoClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.config.AbstractReactiveMongoConfiguration;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;

// NOTE: I'm using embedded MongoDB with flapdoodle for Reactive Mongo simulation
// See `readme.md` about the 2 ways of simulating Reactive Mongo using installed MongoDB vs embedded MongoDB with flapdoodle
@Configuration
@EnableReactiveMongoRepositories(basePackages = "com.giocosmiano.exploration.repository")
//public class MongoConfig extends AbstractReactiveMongoConfiguration {
public class MongoConfig {

    private MongoYamlConfig mongoYamlConfig;

    MongoConfig(MongoYamlConfig mongoYamlConfig) {
        this.mongoYamlConfig = mongoYamlConfig;
    }

/*
    @Bean
    public MongoClient reactiveMongoClient() {
        // mongodb://myDBReader:D1fficultP40ssw0rd@mongodb0.example.com:27017/myDB?authSource=admin
        String uri =
                String.format("mongodb://%s:%s@%s:%s/%s"
                        , mongoYamlConfig.getUsername()
                        , mongoYamlConfig.getPassword()
                        , mongoYamlConfig.getHost()
                        , mongoYamlConfig.getPort()
                        , mongoYamlConfig.getDatabase()
                );
        return MongoClients.create(uri);
    }

    @Override
    protected String getDatabaseName() {
        return mongoYamlConfig.getDatabase();
    }

    @Bean
    public ReactiveMongoTemplate reactiveMongoTemplate() {
        return new ReactiveMongoTemplate(reactiveMongoClient(), getDatabaseName());
    }
*/
}