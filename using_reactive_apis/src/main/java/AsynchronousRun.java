import io.reactivex.BackpressureStrategy;
import io.reactivex.Flowable;
import io.reactivex.FlowableEmitter;
import io.reactivex.schedulers.Schedulers;

public class AsynchronousRun {
  public static void main(String[] args) {
    Flowable.<Integer>create(emitter -> emit(emitter), BackpressureStrategy.BUFFER)
        .map(data -> data * 1.0)
        .filter(data -> data > 4)
        .observeOn(Schedulers.io())
        .subscribe(AsynchronousRun::printIt,
            err -> System.out.println("ERROR: " + err),
            () -> System.out.println("DONE"));
  }

  private static void printIt(Double value) throws InterruptedException {
    System.out.println(value + " -- " + Thread.currentThread());
    Thread.sleep(1000);
  }

  private static void emit(FlowableEmitter<Integer> emitter) throws InterruptedException {
    int count = 0;

    while(count < 20) {
      System.out.println("emitting " + count + " --" + Thread.currentThread());
      emitter.onNext(count++);

      Thread.sleep(500);
    }
  }
}