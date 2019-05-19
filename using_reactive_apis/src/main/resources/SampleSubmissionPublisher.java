import static java.util.concurrent.Flow.*;

import java.util.concurrent.SubmissionPublisher;

class PrintResult implements Subscriber<Integer> {
  private Subscription subscription;

  @Override
  public void onSubscribe(Subscription subscription) {
    System.out.println("subscribed...");
    this.subscription = subscription;
    subscription.request(5);
  }

  @Override
  public void onNext(Integer data) {
    System.out.println(data);
    subscription.request(1);
    if(data > 9) subscription.cancel();
  }

  @Override
  public void onError(Throwable throwable) {

  }

  @Override
  public void onComplete() {

  }
}

public class SampleSubmissionPublisher {
  public static void main(String[] args) throws InterruptedException {
    SubmissionPublisher<Integer> publisher =
        new SubmissionPublisher<>();

    PrintResult subscriber = new PrintResult();
    publisher.subscribe(subscriber);

    int count = 0;
    while(count < 20) {
      Thread.sleep(500);
      if(!publisher.hasSubscribers()) {
        System.out.println("getting out");
        break;
      }
      count++;
      publisher.submit(count);
    }

    Thread.sleep(2000);
  }
}