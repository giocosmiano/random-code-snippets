package com.giocosmiano.exploration.service

import java.util
import java.util.{ArrayList, List}
import java.util.concurrent.atomic.AtomicReference

import com.giocosmiano.exploration.domain.HotVsColdEither
import com.giocosmiano.exploration.reactiveApis.HotVsColdObservablesScala
import io.reactivex.Scheduler
import org.springframework.http.ResponseEntity
import rx.lang.scala.Observable

class HotVsColdObservableServiceScala {

  def getObservablePrimesFromScala(isHotObservable: Boolean
                                   , threshold: Integer): Observable[ResponseEntity[util.List[HotVsColdEither]]] = {

    val atomicReference = new AtomicReference[util.List[HotVsColdEither]]
    atomicReference.set(new util.ArrayList[HotVsColdEither])

    new HotVsColdObservablesScala()
      .runObservable(isHotObservable, threshold)
      .map((either: HotVsColdEither) => {
        atomicReference.get.add(either)
        atomicReference.get
      })
      .last
      .single
      .map(listOfPrimes => ResponseEntity.ok(listOfPrimes))
  }


}

