import kotlinx.coroutines.*

fun doWork(n: Int) {
  println("doWork $n: called from ${Thread.currentThread()}")
}

runBlocking<Unit> {
  doWork(1)
  
  GlobalScope.launch {
    doWork(2)
    yield()
    doWork(3)
  }         
  
  launch {
    doWork(4)
  }
}

/*
doWork 1: called from Thread[main,5,main]
doWork 2: called from Thread[DefaultDispatcher-worker-1,5,main]
doWork 3: called from Thread[DefaultDispatcher-worker-3,5,main]
doWork 4: called from Thread[main,5,main]
*/