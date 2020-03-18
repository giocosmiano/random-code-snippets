import java.util.concurrent.atomic.AtomicInteger

import rx.lang.scala.{Observable, Observer, Subscription}

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.Future
import scala.util.{Failure, Success}

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

  def doOnNext(isHotObservable: Boolean, subscriberNbr: Int, future: Future[Int]): Unit = {
    future onComplete {
      case Success(data) =>
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

      case Failure(e) =>
        println(
          s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
            + s"${if (subscriberNbr == 1) "Subscriber 1: " + e.getMessage else ""}"
            + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + e.getMessage else ""}"
            + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + e.getMessage else ""}"
            + "\t - from doOnNext()"
        )
    }
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
      Future {
        Thread.sleep(100)
        observer.onCompleted()
      }

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
        .switchMap[Future[Int]](prime => {

          // Simulating a non-blocking IO e.g. ReST call, but for now just doubling the prime value
          val f = Future {
            Thread.sleep(100)
            prime * 2 // double the value
          }
          val disposableStream$ = Observable.just(f)

          disposableStream$
            .map(future => {
              future map {
                data =>
                  if (data >= 100 && data <= 200)
                    throw new RuntimeException(s"Simulating an error skipping prime=$data, in-between 100 and 200, while continue streaming the rest")

                  // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just setting it back to original prime
                  Thread.sleep(100)
                  data / 2 // set it back to original `prime` after doubling the value
              } recover {
                case e => throw new RuntimeException(e.getMessage)
              }
            })
            .onErrorReturn(error => {
              println(error.getMessage)
              Future {
                Thread.sleep(100)
                0
              }
            })
        })

    if (isHotObservable) observable = observable.share

    val onNext = (subscriberNbr: Int, future: Future[Int]) => doOnNext(isHotObservable, subscriberNbr, future)
    val onError = (subscriberNbr: Int, error: Throwable) => doOnError(isHotObservable, subscriberNbr, error)
    val onComplete = (subscriberNbr: Int, subscription: Subscription) => doOnComplete(isHotObservable, subscriberNbr, subscription)

    var subscription1: Subscription = null
    var subscription2: Subscription = null
    var subscription3: Subscription = null

    subscription1 =
      observable
        .subscribe(
          future => onNext(1, future)
          , error => onError(1, error)
          , () => onComplete(1, subscription1)
        )

    Thread.sleep(2000)
    subscription2 =
      observable
        .subscribe(
          future => onNext(2, future)
          , error => onError(2, error)
          , () => onComplete(2, subscription2)
        )

    Thread.sleep(2000)
    subscription3 =
      observable
        .subscribe(
          future => onNext(3, future)
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