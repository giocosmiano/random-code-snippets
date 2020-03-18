import java.util.concurrent.atomic.AtomicInteger

import rx.lang.scala.{Observable, Observer, Subscription}

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.Future

// the following is equivalent to `implicit val ec = ExecutionContext.global`
import scala.concurrent.ExecutionContext.Implicits.global

object HotVsColdObservablesScala extends App {
  val subscriber1 = new AtomicInteger
  val subscriber2 = new AtomicInteger
  val subscriber3 = new AtomicInteger

  runObservable(false)

  def isPrime(number: Int): Boolean = {
    for (i <- 2 until number) {
      if (number % i == 0) return false
    }
    true
  }

  def getNextPrime(number: Int): Int = {
    val start = number + 1

    @tailrec
    def chkIsPrime(nbr: Int): Int = {
      if (isPrime(nbr))
        return nbr
      chkIsPrime(nbr + 1)
    }

    chkIsPrime(start)
  }

  def doOnNext(isHotObservable: Boolean, subscriberNbr: Int, data: Int): Unit = {
    subscriberNbr match {
      case 1 => subscriber1.set(data)
      case 2 => subscriber2.set(data)
      case 3 => subscriber3.set(data)
      case _ => Unit
    }

    println(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        + s"${if (subscriberNbr == 1) "Subscriber 1: " + subscriber1.get else ""}"
        + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + subscriber2.get else ""}"
        + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + subscriber3.get else ""}"
        + "\t - from doOnNext()"
    )
  }

  def doOnError(isHotObservable: Boolean, subscriberNbr: Int, error: Throwable): Unit = {
    println(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        + s"${if (subscriberNbr == 1) "Subscriber 1: " + error.getMessage else ""}"
        + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + error.getMessage else ""}"
        + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + error.getMessage else ""}"
        + "\t - from doOnError()"
    )
  }

  def doOnComplete(isHotObservable: Boolean, subscriberNbr: Int, subscription: Subscription): Unit = {
    subscription.unsubscribe()

    println(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        + s"${if (subscriberNbr == 1) "Subscriber 1: " + subscriber1.get else ""}"
        + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + subscriber2.get else ""}"
        + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + subscriber3.get else ""}"
        + "\t - from doOnComplete()"
    )
  }

  def nextPrime(number: Int, observer: Observer[Int]): Unit = {
    val prime = getNextPrime(number)

    // Emit a completion when threshold is reached
    if (prime >= 500) {
      observer.onCompleted()

      // https://github.com/ReactiveX/RxJava/wiki/Error-Handling
      // https://github.com/ReactiveX/RxJava/wiki/What's-different-in-2.0#error-handling
      // un-comment to simulate an `onError` that will halt the entire stream of data
//    } else if (prime >= 200) {
//      observer.onError(new RuntimeException("Simulating an error that will halt the entire stream of data. Data=" + prime))

      // emit the next data
    } else {
      observer.onNext(prime)
      Future {
        Thread.sleep(100)
        nextPrime(prime, observer)
      }
    }
  }

  def runObservable(isHotObservable: Boolean): Unit = {
    println(s"Starting ${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName}")

    // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
    // http://localhost:8080/getEmployeeDetails/123
    // CompletableFuture.supplyAsync(() -> getEmployee(empId))
    //         .thenApply(emp -> getEmployeeDept(empId))
    //         .thenApply(emp -> getEmployeePay(empId))
    var observable =
      Observable.create[Int](observer => {
        nextPrime(1, observer)
        Subscription()
      })
        .switchMap[Int](prime => {
          val disposableStream$ = Observable.just(prime)

          disposableStream$
            .map(data => {
              if (data >= 100 && data <= 200)
                throw new RuntimeException(s"Simulating an error skipping prime=$prime, in-between 100 and 200, while continue streaming the rest")
              data
            })
            .onErrorReturn(error => {
              println(error.getMessage)
              0
            })
        })

    if (isHotObservable) observable = observable.share

    val onNext = (subscriberNbr: Int, data: Int) => doOnNext(isHotObservable, subscriberNbr, data)
    val onError = (subscriberNbr: Int, error: Throwable) => doOnError(isHotObservable, subscriberNbr, error)
    val onComplete = (subscriberNbr: Int, subscription: Subscription) => doOnComplete(isHotObservable, subscriberNbr, subscription)

    var subscription1: Subscription = null
    var subscription2: Subscription = null
    var subscription3: Subscription = null

    subscription1 =
      observable
        .subscribe(
          data => onNext(1, data)
          , error => onError(1, error)
          , () => onComplete(1, subscription1)
        )

    Thread.sleep(2000)
    subscription2 =
      observable
        .subscribe(
          data => onNext(2, data)
          , error => onError(2, error)
          , () => onComplete(2, subscription2)
        )

    Thread.sleep(2000)
    subscription3 =
      observable
        .subscribe(
          data => onNext(3, data)
          , error => onError(3, error)
          , () => onComplete(3, subscription3)
        )


    var anySubscribersStillListening = false
    do {
      anySubscribersStillListening =
        (
          (subscription1 != null && ! subscription1.isUnsubscribed)
            || (subscription2 != null && ! subscription2.isUnsubscribed)
            || (subscription3 != null && ! subscription3.isUnsubscribed)
          )
      Thread.sleep(1000)
    } while ( anySubscribersStillListening )

    println(s"DONE with ${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName}")
  }
}