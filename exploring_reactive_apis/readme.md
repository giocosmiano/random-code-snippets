### Exploring RxJava vs Reactor Flux/Mono

 - **Simulation tests on Observable/Reactor Flux generating sequence of prime numbers which**
   - demonstrates Hot vs Cold Observable/Flux
   - demonstrates 3-different subscribers on single Observable/Flux
   - sample written in [Reactor Flux v3.2.15](https://projectreactor.io/docs)
   - sample written in [RxJava v2.2.8](https://github.com/ReactiveX/RxJava), [RxGroovy](https://github.com/ReactiveX/RxGroovy) and [RxScala, which is now EOL](https://github.com/ReactiveX/RxScala) 

 - **Simulation tests on Reactor Flux/Mono using Spring's [data](https://spring.io/projects/spring-data)**
   - **[Mongo](https://spring.io/projects/spring-data-mongodb), with its [reactive-DB driver](https://spring.io/blog/2016/11/28/going-reactive-with-spring-data)**
   - **[H2](https://spring.io/projects/spring-data-jpa), with its non-reactive DB driver, in conjunction with Reactor Flux/Mono to be non-blocking IO**
 
 - **2 ways of simulating Reactive Mongo in this project**
   - Using an installed MongoDB
     - Comment the annotations `Component` for `InitDatabase` and `Bean` for `init(MongoOperations operations)` in
       [com.giocosmiano.exploration.clr.InitDatabase](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/src/main/java/com/giocosmiano/exploration/clr/InitDatabase.java)
     - Un-comment the `scope=test` of `de.flapdoodle.embed.mongo` artifact in [POM](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/pom.xml)
     - Un-comment the annotations `EnableReactiveMongoRepositories` for `MongoConfig` and entire block for 2 `Bean`s in
       [com.giocosmiano.exploration.config.MongoConfig](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/src/main/java/com/giocosmiano/exploration/config/MongoConfig.java)
     - Un-comment the `spring.data.mongodb` configs in [application.yaml](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/src/main/resources/application.yaml)
   
   - Using Embedded MongoDB flapdoodle, if there isn't an installed MongoDB
     - Un-comment the annotations `Component` for `InitDatabase` and `Bean` for `init(MongoOperations operations)` in
       [com.giocosmiano.exploration.clr.InitDatabase](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/src/main/java/com/giocosmiano/exploration/clr/InitDatabase.java)
     - Comment the `scope=test` of `de.flapdoodle.embed.mongo` artifact in [POM](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/pom.xml)
     - Comment the annotations `EnableReactiveMongoRepositories` for `MongoConfig` and entire block for 2 `Bean`s in
       [com.giocosmiano.exploration.config.MongoConfig](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/src/main/java/com/giocosmiano/exploration/config/MongoConfig.java)
     - Comment the `spring.data.mongodb` configs in [application.yaml](https://github.com/giocosmiano/random-code-snippets/blob/master/exploring_reactive_apis/src/main/resources/application.yaml)

 - Sample Mongo data set are from this [site](https://github.com/ozlerhakan/mongodb-json-files) 

 - **Reading references**
   - [MS Open Tech Open Sources Rx (Reactive Extensions) – a Cure for Asynchronous Data Streams in Cloud Programming](https://blogs.msdn.microsoft.com/interoperability/2012/11/06/ms-open-tech-open-sources-rx-reactive-extensions-a-cure-for-asynchronous-data-streams-in-cloud-programming/)
   - [Reactive Programming in the Netflix API with RxJava](https://netflixtechblog.com/reactive-programming-in-the-netflix-api-with-rxjava-7811c3a1496a) 
   - [Reactive Programming at Netflix](https://netflixtechblog.com/reactive-programming-at-netflix-b944d49874d2)
   - [Optimizing the Netflix API](https://netflixtechblog.com/optimizing-the-netflix-api-5c9ac715cf19)
   - [Spring WebFlux](https://howtodoinjava.com/spring-webflux/spring-webflux-tutorial/)
 

















