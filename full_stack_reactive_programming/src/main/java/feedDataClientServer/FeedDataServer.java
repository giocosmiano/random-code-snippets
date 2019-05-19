package feedDataClientServer;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.reactivex.core.AbstractVerticle;
import io.vertx.reactivex.core.http.HttpServer;
import io.vertx.reactivex.core.streams.ReadStream;
import io.vertx.reactivex.core.http.HttpServerRequest;
import io.vertx.reactivex.core.http.HttpServerResponse;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;
import io.reactivex.Flowable;
import java.util.concurrent.TimeUnit;
import com.mongodb.reactivestreams.client.MongoClients;
import com.mongodb.reactivestreams.client.MongoClient;
import com.mongodb.reactivestreams.client.MongoDatabase;
import com.mongodb.reactivestreams.client.MongoCollection; 
import org.bson.Document;
import io.vertx.core.json.Json;
import java.util.concurrent.atomic.AtomicReference;
                                                             
public class FeedDataServer extends AbstractVerticle {
  @Override
  public void start(Future<Void> startFuture) throws Exception {
    final HttpServer httpServer = vertx.createHttpServer();

    processRequest(httpServer.requestStream());

    httpServer.listen(8080, handler -> listen(handler, startFuture));
  }

  private void listen(AsyncResult<HttpServer> handler, Future<Void> startFuture) {
    if(handler.succeeded()) {
      startFuture.complete();
      System.out.println("Severver started...");
    } else {
      System.out.println("failure " + handler.cause());
      startFuture.fail(handler.cause());
    }
  }

  private void processRequest(ReadStream<HttpServerRequest> httpServerRequestStream) {
    httpServerRequestStream.toFlowable()
      .subscribe(request -> handleRequest(request));
  }

  private void handleRequest(HttpServerRequest request) {
    final HttpServerResponse response = request.response();
    response.setChunked(true);

    MongoClient mongoClient = MongoClients.create();
    MongoDatabase mongoDatabase = mongoClient.getDatabase("todo");
    MongoCollection<Document> collection = mongoDatabase.getCollection("tasks");

    AtomicReference<Subscription> dataSubscription = new AtomicReference<>();                   

    collection.find()
      .subscribe(new Subscriber<Document>() {
        public void onSubscribe(Subscription subscription) {
          dataSubscription.set(subscription);
        }

        public void onNext(Document document) {
          System.out.println("sending... " + document.get("name").toString());

          response.write(String.format("{name: %s, status: %s}", 
    				document.get("name"), document.get("status")));
        }                              

        public void onError(Throwable throwable) {
          System.out.println("ERROR: " + throwable);
        }                           

        public void onComplete() {
          response.end();
        }
      });
      
      request.toFlowable()
        .subscribe(
          requestData -> {
            dataSubscription.get().request(Integer.valueOf(requestData.toString()));
          },
          err -> System.out.println("ERROR: " + err),
          () -> response.end());
  }
}