package com.giocosmiano.exploration.reactiveApis

import java.util.concurrent.atomic.AtomicInteger

import com.giocosmiano.exploration.reactiveApis.HotVsColdObservablesScala._
import org.slf4j.{Logger, LoggerFactory}
import rx.lang.scala.{Observable, Observer, Subscription}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Either, Failure, Left, Right, Success}

// the following is equivalent to `implicit val ec = ExecutionContext.global`
import scala.concurrent.ExecutionContext.Implicits.global

class HotVsColdObservablesScala {

  // NOTE: RxScala has been EOL
  // https://github.com/ReactiveX/RxScala
  // https://github.com/ReactiveX/RxScala/issues/244
  def runObservable(isHotObservable: Boolean = DEFAULT_COLD_OBSERVABLE
                    , threshold: Int = DEFAULT_THRESHOLD): Observable[Future[Either[String,Int]]] = {

    val observable: Observable[Future[Either[String,Int]]] =
    createObservable(isHotObservable, threshold)
      .map(future => doubleThePrime(future))
      .map(future => resetThePrime(future))
//      .doOnNext((either: HotVsColdEither) => {
//        log.info(
//          s"Observable from ${Thread.currentThread.getName} - \t"
//            + s"${if (either.getRightValue != null) "Value : " + either.getRightValue else ""}"
//            + s"${if (either.getLeftValue != null) "\tError : " + either.getLeftValue else ""}"
//            + "\t - from doOnNext()"
//        )
//      })
    observable
  }
}

object HotVsColdObservablesScala {

  val START_PRIME_AT_1 = 1
  val SUBSCRIBER_NBR_1 = 1
  val SUBSCRIBER_NBR_2 = 2
  val SUBSCRIBER_NBR_3 = 3
  val DEFAULT_THRESHOLD = 500
  val DEFAULT_COLD_OBSERVABLE: Boolean = false

  val subscriber1 = new AtomicInteger
  val subscriber2 = new AtomicInteger
  val subscriber3 = new AtomicInteger

  val log = LoggerFactory.getLogger(classOf[HotVsColdObservablesScala])

  // NOTE: This is a simulation of a 3-Subscribers from 1-Observable
  def main(args: Array[String]): Unit = {
    val isHotObservable = DEFAULT_COLD_OBSERVABLE
    val threshold = DEFAULT_THRESHOLD
    val mapOfDisposable = mutable.Map.empty[Int, Subscription]

    log.info(s"Starting ${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName}")

    val observable = createObservable(isHotObservable, threshold)

    mapOfDisposable += SUBSCRIBER_NBR_1 -> runObservableForSubscriberNbr(isHotObservable, SUBSCRIBER_NBR_1, observable)

    Thread.sleep(2000)
    mapOfDisposable += SUBSCRIBER_NBR_2 -> runObservableForSubscriberNbr(isHotObservable, SUBSCRIBER_NBR_2, observable)

    Thread.sleep(2000)
    mapOfDisposable += SUBSCRIBER_NBR_3 -> runObservableForSubscriberNbr(isHotObservable, SUBSCRIBER_NBR_3, observable)

    var anySubscribersStillListening = false
    do {
      val disposables = mapOfDisposable.values.filter(e => e != null).filter(e => ! e.isUnsubscribed)

      anySubscribersStillListening = disposables.nonEmpty
      Thread.sleep(1000)
    } while ( anySubscribersStillListening )

    log.info(s"DONE with ${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName}")
  }

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

  def doOnNext(isHotObservable: Boolean, subscriberNbr: Int, future: Future[Either[String,Int]]): Unit = {
    future onComplete {

      case Success(either) =>
        var message = ""

        if (either.isRight) {
          val data = either.right.get
          message = data.toString
          subscriberNbr match {
            case 1 => subscriber1.set(data)
            case 2 => subscriber2.set(data)
            case 3 => subscriber3.set(data)
            case _ => Unit
          }

        } else {
          message = either.left.get
        }

        log.info(
          s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
            + s"${if (subscriberNbr == 1) "Subscriber 1: " + message else ""}"
            + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + message else ""}"
            + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + message else ""}"
            + "\t - from doOnNext()"
        )

      case Failure(e) =>
        log.info(
          s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
            + s"${if (subscriberNbr == 1) "Subscriber 1: " + e.getMessage else ""}"
            + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + e.getMessage else ""}"
            + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + e.getMessage else ""}"
            + "\t - from doOnNext()"
        )
    }
  }

  def doOnError(isHotObservable: Boolean, subscriberNbr: Int, error: Throwable): Unit = {
    log.info(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        + s"${if (subscriberNbr == 1) "Subscriber 1: " + error.getMessage else ""}"
        + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + error.getMessage else ""}"
        + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + error.getMessage else ""}"
        + "\t - from doOnError()"
    )
  }

  def doOnComplete(isHotObservable: Boolean, subscriberNbr: Int, subscription: Subscription): Unit = {
    subscription.unsubscribe()

    log.info(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        + s"${if (subscriberNbr == 1) "Subscriber 1: " + subscriber1.get else ""}"
        + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + subscriber2.get else ""}"
        + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + subscriber3.get else ""}"
        + "\t - from doOnComplete()"
    )
  }

  def nextPrime(number: Int, threshold: Int, observer: Observer[Int]): Unit = {
    val prime = getNextPrime(number)

    // Emit a completion when threshold is reached
    if (prime >= threshold) {
      Future {
        Thread.sleep(500)
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
        nextPrime(prime, threshold, observer)
      }
    }
  }

  // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just applying a timeout and doubling the value
  val doubleThePrime: Future[Either[String,Int]] => Future[Either[String,Int]] = future => {
    future map { either =>

      Thread.sleep(100)
      val data = either.right.get
      val newValue = data * 2 // double the value

      // Simulating an error using Either.left()
      if (newValue >= 100 && newValue <= 200) {
        val error = s"Simulating an error skipping double value of prime in-between 100 and 200, where prime=$data and double=$newValue"
        Left(error)

      } else Right(newValue)
    }
  }

  // Simulating a non-blocking IO e.g. ReST call, but for now just applying a timeout and setting it back to original prime
  val resetThePrime: Future[Either[String,Int]] => Future[Either[String,Int]] = future => {
    future map { either =>

      Thread.sleep(100)

      if (either.isRight) {
        val data = either.right.get
        val newValue = data / 2
        Right(newValue)

      } else either
    }
  }

  // NOTE: This is for a simulation of a 3-Subscribers from 1-Observable, from main()
  def runObservableForSubscriberNbr(isHotObservable: Boolean
                                    , subscriberNbr: Int
                                    , observable: Observable[Future[Either[String,Int]]]
                                   ): Subscription = {

    val onNext = (subscriberNbr: Int, future: Future[Either[String,Int]]) => doOnNext(isHotObservable, subscriberNbr, future)
    val onError = (subscriberNbr: Int, error: Throwable) => doOnError(isHotObservable, subscriberNbr, error)
    val onComplete = (subscriberNbr: Int, subscription: Subscription) => doOnComplete(isHotObservable, subscriberNbr, subscription)

    var subscription: Subscription = null
    subscription =
      observable
        .map(future => doubleThePrime(future))
        .map(future => resetThePrime(future))
        .subscribe(
          future => onNext(subscriberNbr, future)
          , error => onError(subscriberNbr, error)
          , () => onComplete(subscriberNbr, subscription)
        )
    subscription
  }

  def createObservable(isHotObservable: Boolean
                       , threshold: Int): Observable[Future[Either[String,Int]]] = {
    // Simulating a non-blocking IO such as a ReST call then a Reactive Mongo chaining them up together e.g.
    // http://localhost:8080/getEmployeeDetails/123
    // CompletableFuture.supplyAsync(() -> getEmployee(empId))
    //         .thenApply(emp -> getEmployeeDept(empId))
    //         .thenApply(emp -> getEmployeePay(empId))
    var observable =
    Observable.create[Int](observer => {
      nextPrime(START_PRIME_AT_1, threshold, observer)
      Subscription()
    })
      .switchMap[Future[Either[String,Int]]](prime => {

        // Simulation # 1 - a non-blocking IO using doubleThePrime and resetThePrime functions below
        val f = Future {
          Thread.sleep(100)
          Right(prime)
        }
        val disposableStream$ = Observable.just(f)
        disposableStream$

/*
        // NOTE: un-comment if want to try different simulation without using doubleThePrime and resetThePrime functions below
        // Simulation # 2 - a non-blocking IO e.g. ReST call, but for now just doubling the prime value
        val f = Future {
          Thread.sleep(100)
          Right(prime * 2) // double the value
        }
        val disposableStream$ = Observable.just(f)

        disposableStream$
          .map(future => {
            future map {
              either =>

                // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a timeout and setting it back to original prime
                Thread.sleep(100)
                val data = either.right.get
                val prime = data / 2

                // Simulating an error using Either.left()
                if (data >= 100 && data <= 200) {
                  val error = s"Simulating an error skipping double value of prime in-between 100 and 200, where prime=$prime and double=$data"
                  Left(error)

                } else {
                  Right(data / 2) // set it back to original `prime` after doubling the value
                }
            }
          })
*/
      })

    if (isHotObservable) observable = observable.share

    observable
  }
}