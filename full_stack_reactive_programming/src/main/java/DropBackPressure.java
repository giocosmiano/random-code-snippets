import io.reactivex.BackpressureStrategy;
import io.reactivex.Flowable;
import io.reactivex.FlowableEmitter;
import io.reactivex.schedulers.Schedulers;

public class DropBackPressure {
  public static void main(String[] args) throws InterruptedException {
    Flowable.<Integer>create(emitter -> emit(emitter),
        BackpressureStrategy.DROP)
      .map(data -> data * 1.0)
      .observeOn(Schedulers.computation(), true, 2)
      .subscribe(DropBackPressure::printIt,
          System.out::println, () -> System.out.println("DONE"));

    Thread.sleep(30000);
  }

  private static void printIt(Double value) {
    try {
      Thread.sleep(1000);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    System.out.println(value);
  }

  private static void emit(FlowableEmitter<Integer> emitter) {
    int count = 0;

    while(count < 10) {
      count++;
      System.out.println("emitting... " + count);
      try {
        Thread.sleep(500);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
      emitter.onNext(count);
    }

    emitter.onComplete();
  }
}