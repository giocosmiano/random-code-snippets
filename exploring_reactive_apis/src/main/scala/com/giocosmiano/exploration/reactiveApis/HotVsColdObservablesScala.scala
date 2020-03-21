package com.giocosmiano.exploration.reactiveApis

import java.util.concurrent.atomic.AtomicInteger

import rx.lang.scala.{Observable, Observer, Subscription}

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.{Either, Failure, Left, Right, Success}

// the following is equivalent to `implicit val ec = ExecutionContext.global`
import scala.concurrent.ExecutionContext.Implicits.global

object HotVsColdObservablesScala extends App {
  val subscriber1 = new AtomicInteger
  val subscriber2 = new AtomicInteger
  val subscriber3 = new AtomicInteger

  runObservable(false)
//  runObservableWithManualErrorHandling(false)

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

        println(
          s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
            + s"${if (subscriberNbr == 1) "Subscriber 1: " + message else ""}"
            + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + message else ""}"
            + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + message else ""}"
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

  def doOnNext2(isHotObservable: Boolean, subscriberNbr: Int, future: Future[Int]): Unit = {
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
        nextPrime(prime, observer)
      }
    }
  }

  /*
   * Using `Either` to continuously stream data with errors, either with the value on right or error on left
   */
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
        .switchMap[Future[Either[String,Int]]](prime => {

          // Simulation # 1 - a non-blocking IO using doubleIt and resetIt functions below
          val f = Future {
            Thread.sleep(100)
            Right(prime)
          }
          val disposableStream$ = Observable.just(f)
          disposableStream$

/*
          // NOTE: un-comment if want to try different simulation without using doubleIt and resetIt functions below
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

    // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just applying a timeout and doubling the value
    val doubleIt: (Int, Future[Either[String,Int]]) => Future[Either[String,Int]] = (subscriberNbr, future) => {
      future map { either =>

        Thread.sleep(100)
        val data = either.right.get
        val newValue = data * 2 // double the value

//        println(
//          s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
//            + s"${if (subscriberNbr == 1) "Subscriber 1: from " + data + " to " + newValue else ""}"
//            + s"${if (subscriberNbr == 2) "\tSubscriber 2: from " + data + " to " + newValue else ""}"
//            + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: from " + data + " to " + newValue else ""}"
//            + "\t - from doubleIt()"
//        )

        // Simulating an error using Either.left()
        if (newValue >= 100 && newValue <= 200) {
          val error = s"Simulating an error skipping double value of prime in-between 100 and 200, where prime=$data and double=$newValue"
          Left(error)

        } else Right(newValue)
      }
    }

    // Simulating a non-blocking IO e.g. ReST call, but for now just applying a timeout and setting it back to original prime
    val resetIt: (Int, Future[Either[String,Int]]) => Future[Either[String,Int]] = (subscriberNbr, future) => {
      future map { either =>

        Thread.sleep(100)

        if (either.isRight) {
          val data = either.right.get
          val newValue = data / 2
//          println(
//            s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
//              + s"${if (subscriberNbr == 1) "Subscriber 1: from " + data + " to " + newValue else ""}"
//              + s"${if (subscriberNbr == 2) "\tSubscriber 2: from " + data + " to " + newValue else ""}"
//              + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: from " + data + " to " + newValue else ""}"
//              + "\t - from resetIt()"
//          )
          Right(newValue)

        } else either
      }
    }

    val onNext = (subscriberNbr: Int, future: Future[Either[String,Int]]) => doOnNext(isHotObservable, subscriberNbr, future)
    val onError = (subscriberNbr: Int, error: Throwable) => doOnError(isHotObservable, subscriberNbr, error)
    val onComplete = (subscriberNbr: Int, subscription: Subscription) => doOnComplete(isHotObservable, subscriberNbr, subscription)

    var subscription1: Subscription = null
    var subscription2: Subscription = null
    var subscription3: Subscription = null

    subscription1 =
      observable
        .map(future => doubleIt(1, future))
        .map(future => resetIt(1, future))
        .subscribe(
          future => onNext(1, future)
          , error => onError(1, error)
          , () => onComplete(1, subscription1)
        )

    Thread.sleep(2000)
    subscription2 =
      observable
        .map(future => doubleIt(2, future))
        .map(future => resetIt(2, future))
        .subscribe(
          future => onNext(2, future)
          , error => onError(2, error)
          , () => onComplete(2, subscription2)
        )

    Thread.sleep(2000)
    subscription3 =
      observable
        .map(future => doubleIt(3, future))
        .map(future => resetIt(3, future))
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

  def runObservableWithManualErrorHandling(isHotObservable: Boolean): Unit = {
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

                // Simulating a non-blocking IO e.g. Reactive Mongo, but for now just a timeout and setting it back to original prime
                Thread.sleep(100)

                // Simulating an error
                if (data >= 100 && data <= 200) {
                  val error = s"Simulating an error skipping prime=$data, in-between 100 and 200, while continue streaming the rest"
                  throw new RuntimeException(error)
                }

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

    val onNext = (subscriberNbr: Int, future: Future[Int]) => doOnNext2(isHotObservable, subscriberNbr, future)
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