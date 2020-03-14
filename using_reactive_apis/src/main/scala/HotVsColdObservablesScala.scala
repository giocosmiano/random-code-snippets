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

  def doOnNext(isHotObservable: Boolean, subscriberNbr: Int, data: Int): Unit =
    println(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        +  s"${if (subscriberNbr == 1) "Subscriber 1: " + data else ""}"
        +  s"${if (subscriberNbr == 2) "\tSubscriber 2: " + data else ""}"
        +  s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + data else ""}"
//        +  "\t - from doOnNext()"
    )

  def doubleTheValue(subscriberNbr: Int, data: Int): Int = {
    val doubleIt = data * 2
    subscriberNbr match {
      case 1 => subscriber1.set(doubleIt)
      case 2 => subscriber2.set(doubleIt)
      case 3 => subscriber3.set(doubleIt)
      case _ => Unit
    }
    doubleIt
  }

  def printSubscribeMessage(isHotObservable: Boolean, subscriberNbr: Int): Unit = ""

  def printSubscribeMessageX(isHotObservable: Boolean, subscriberNbr: Int): Unit =
    println(
      s"${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName} - \t"
        + s"${if (subscriberNbr == 1) "Subscriber 1: " + subscriber1.get else ""}"
        + s"${if (subscriberNbr == 2) "\tSubscriber 2: " + subscriber2.get else ""}"
        + s"${if (subscriberNbr == 3) "\t\tSubscriber 3: " + subscriber3.get else ""}"
        + "\t - from printSubscribeMessage()"
    )

  def nextPrime(number: Int, observer: Observer[Int]): Unit = {
    val prime = getNextPrime(number)
    observer.onNext(prime)
    Future {
      Thread.sleep(100)
      nextPrime(prime, observer)
    }
  }

  def runObservable(isHotObservable: Boolean): Unit = {
    println(s"Starting ${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName}")

    var observable = Observable.create[Int](observer => {
      nextPrime(1, observer)
      Subscription()
    })

    if (isHotObservable) observable = observable.share

    val disposable1 =
      observable.doOnNext(data => doOnNext(isHotObservable,1, data))
        .map(data => doubleTheValue(1, data))
        .subscribe(_ => printSubscribeMessage(isHotObservable,1))
    Thread.sleep(2000)

    val disposable2 =
      observable.doOnNext(data => doOnNext(isHotObservable,2, data))
        .map(data => doubleTheValue(2, data))
        .subscribe(_ => printSubscribeMessage(isHotObservable,2))
    Thread.sleep(3000)

    val disposable3 =
      observable.doOnNext(data => doOnNext(isHotObservable,3, data))
        .map(data => doubleTheValue(3, data))
        .subscribe(_ => printSubscribeMessage(isHotObservable,3))
    Thread.sleep(4000)

    disposable1.unsubscribe()
    disposable2.unsubscribe()
    disposable3.unsubscribe()

    Thread.sleep(5000)
    println(s"DONE with ${if (isHotObservable) "Hot" else "Cold"} Observables from ${Thread.currentThread.getName}")
  }
}