import kotlinx.coroutines.*

suspend fun task1() {
  println("Enter task1")
  yield()
  println("Exit task1")
}

suspend fun task2() {
  println("Enter task2")
  yield()
  println("Exit task2")
}

runBlocking {
  launch { task1() }
  launch { task2() }
  
  println("invoked tasks")
}

/*
Parallel: Two or more tasks happen at the same time
Concurrent: Two or more tasks are interleaved
*/