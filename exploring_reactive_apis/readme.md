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

 - **Extra stuff - [JWT](https://jwt.io/) exploration using [JJWT](https://github.com/jwtk/jjwt)**
   - [JJWT – JSON Web Token for Java and Android](https://stormpath.com/blog/jjwt-how-it-works-why)
   - [A Beginner’s Guide to JWTs in Java](https://stormpath.com/blog/beginners-guide-jwts-in-java)
   - [Supercharge Java Authentication with JSON Web Tokens (JWTs)](https://www.baeldung.com/java-json-web-tokens-jjwt)
   - [Create and Verify JWTs in Java](https://developer.okta.com/blog/2018/10/31/jwts-with-java)

 - [Spring Externalized Configuration](https://docs.spring.io/spring-boot/docs/current/reference/html/spring-boot-features.html#boot-features-external-config) 
   - [Spring Externalized Configuration v.1.5.22](https://docs.spring.io/spring-boot/docs/1.5.22.RELEASE/reference/html/boot-features-external-config.html) 
 - [Spring Profile-specific Properties](https://docs.spring.io/spring-boot/docs/current/reference/html/spring-boot-features.html#boot-features-external-config-profile-specific-properties)
   - Overriding spring profile/property at runtime, such as overriding property `jwtSecretKey` e.g.
     - java -jar -Dspring.profiles.active=dev ./exploring_reactive_apis-1.0-SNAPSHOT.jar --jwtSecretKey="Th1s1sAV3ryL0ngSecretKeyIndeed!!$"

 - **Extra stuff - Simulation to demonstrate eagerness of `Mono.just()` vs laziness of `Mono.fromCallable()` and `Mono.defer()` using UUIDGenerator**
   -  [Mono.just()](https://projectreactor.io/docs/core/release/api/reactor/core/publisher/Mono.html#just-T-)
   -  [Mono.fromCallable()](https://projectreactor.io/docs/core/release/api/reactor/core/publisher/Mono.html#fromCallable-java.util.concurrent.Callable-)
   -  [Mono.defer()](https://projectreactor.io/docs/core/release/api/reactor/core/publisher/Mono.html#defer-java.util.function.Supplier-)

 - Spring Webflux download process
   - [Spring webflux file upload and download](https://ddcode.net/2019/06/21/spring-webflux-file-upload-and-download/)
   - [Resume file downloading with Spring Webflux and static files serving in Spring
](https://stackoverflow.com/questions/58560204/resume-file-downloading-with-spring-webflux-and-static-files-serving-in-spring)
   - [Processing streaming data with Spring WebFlux](https://medium.com/@nithinmallya4/processing-streaming-data-with-spring-webflux-ed0fc68a14de)
   - [Event Streaming Using Spring WebFlux](https://dzone.com/articles/event-streaming-using-spring-webflux)

 - AOP Logging with Context in Spring WebFlux
   - [Reactor Context](https://www.youtube.com/watch?v=5tlZddM5Jo0)
   - [Contextual Logging with Reactor Context and MDC](https://simonbasle.github.io/2018/02/contextual-logging-with-reactor-context-and-mdc/)
   - [Passing Context with Spring WebFlux Part 1](https://ndportmann.com/passing-context-with-spring-webflux/)
   - [Logging with Context in Spring WebFlux Part 2](https://ndportmann.com/logging-with-context-in-spring-webflux/)
   - [Logging in a multithreaded environment and with CompletableFuture construct using MDC](https://medium.com/asyncparadigm/logging-in-a-multithreaded-environment-and-with-completablefuture-construct-using-mdc-1c34c691cef0)
   - [Project Reactor + MDC logging](https://medium.com/@grigorryev/project-reactor-mdc-logging-1047d235ff6e)
   - [Getting Mono context when using Mono.fromCallable](https://stackoverflow.com/questions/51275642/getting-mono-context-when-using-mono-fromcallable)














