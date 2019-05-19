package demoClientServer;

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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.TimeUnit;
                                 
public class DemoServer extends AbstractVerticle {
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

    AtomicInteger count = new AtomicInteger(0);
    
    System.out.println("request received...");                          
    
    request.toFlowable()
      .subscribe(requestData -> emit(response, Integer.valueOf(requestData.toString()), count),
        err -> System.out.println("ERROR: " + err),
        () -> response.end());
  }
  
  private void emit(HttpServerResponse response, int capacity, AtomicInteger count) {
    for(int i = 0; i < capacity; i++) {
      int responseData = count.addAndGet(1);
      System.out.println("Sending..." + responseData);
      response.write(String.valueOf(responseData));
    }    
  }
}