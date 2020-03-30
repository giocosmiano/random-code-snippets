### Exploring RxJava vs Reactor Flux/Mono

 - A sample Observable/Reactor Flux that will generate prime numbers which
   - demonstrates Hot vs Cold 
   - demonstrates with 3-different subscribers
   - can be accessed thru spring-boot ReST 
   - a sample written using Reactor Flux v3.2.15
   - samples written using RxJava v2.2.8, RxGroovy and RxScala is now [EOL](https://github.com/ReactiveX/RxScala) 

 - Sample Mongo data set are from this [site](https://github.com/ozlerhakan/mongodb-json-files) 
 
 - **NOTE: There are 2 ways of simulating Reactive Mongo**
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

















