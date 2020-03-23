package com.giocosmiano.exploration.service

import java.util
import java.util.concurrent.CompletableFuture
import java.util.{ArrayList, List}
import java.util.concurrent.atomic.AtomicReference

import com.giocosmiano.exploration.domain.HotVsColdEither
import com.giocosmiano.exploration.reactiveApis.HotVsColdObservablesScala
import io.reactivex.Scheduler
import org.springframework.http.ResponseEntity
import rx.lang.scala.Observable

import scala.util.Either

import scala.compat.java8.FutureConverters._
import scala.compat.java8.FunctionConverters._

// the following is equivalent to `implicit val ec = ExecutionContext.global`
import scala.concurrent.ExecutionContext.Implicits.global

class HotVsColdObservableServiceScala {

  // NOTE: RxScala has been EOL
  // https://github.com/ReactiveX/RxScala
  // https://github.com/ReactiveX/RxScala/issues/244
  def getObservablePrimesFromScala(isHotObservable: Boolean
                                   , threshold: Integer
                                  ): Observable[util.List[HotVsColdEither]] = {

    val atomicReference = new AtomicReference[util.List[HotVsColdEither]]
    atomicReference.set(new util.ArrayList[HotVsColdEither])

    new HotVsColdObservablesScala()
      .runObservable(isHotObservable, threshold)
      .map(future => {
        future map {
          either => {
            val hotVsColdEither = new HotVsColdEither
            if (either.isRight) {
              hotVsColdEither.setRightValue(either.right.get)
            } else {
              hotVsColdEither.setLeftValue(either.left.get)
            }
            atomicReference.get.add(hotVsColdEither)
            atomicReference
          }
        }
      })
      .map(future => future.toJava.toCompletableFuture)
      .map(promise => promise.get())
      .map(atomicRef => atomicRef.get())
      .last
      .single
  }
}

