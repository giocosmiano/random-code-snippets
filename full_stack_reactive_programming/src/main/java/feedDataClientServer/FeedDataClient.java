package feedDataClientServer;

import io.vertx.core.http.HttpMethod;
import io.vertx.reactivex.core.Vertx;
import io.vertx.reactivex.core.http.HttpClient;
import io.vertx.reactivex.core.http.HttpClientRequest;
import io.vertx.reactivex.core.http.HttpClientResponse;
import io.vertx.reactivex.core.buffer.Buffer;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;
import io.vertx.core.*;

public class FeedDataClient {
  static int PAGE_SIZE = 5;
  static int count = PAGE_SIZE;
  static int totalReceived = 0;

  public static void main(String[] args) {
    final HttpClient httpClient = Vertx.vertx().createHttpClient();

    final HttpClientRequest httpClientRequest = 
      httpClient.request(HttpMethod.GET, 8080, "localhost", "/")
        .setChunked(true);

    httpClientRequest.handler(response -> 
      response.toFlowable()
        .subscribe(data -> processData(data, httpClientRequest),
          err -> System.out.println("ERROR:" + err),
                FeedDataClient::exit));
    
    httpClientRequest.write("" + PAGE_SIZE);
  }
  
  private static void processData(Buffer data, HttpClientRequest httpClientRequest) {
    System.out.println("Received..." + data);
    
    count--;
    totalReceived++;
    try { Thread.sleep(1000); } catch(Exception ex) {}
    
    if(count == 0) {
      if(totalReceived >= 15) {
        System.out.println("...requesting termination...");
        httpClientRequest.end();
      } else {
        count = PAGE_SIZE;
        System.out.println("-----GET MORE-----");
        httpClientRequest.write("" + PAGE_SIZE);
      }
    }
  } 
  
  private static void exit() {
    System.exit(0);
  }
}
