import kotlinx.coroutines.*

fun task1() {
  println("Enter task1")
  println("Exit task1")
}

fun task2() {
  println("Enter task2")
  println("Exit task2")
}

runBlocking {
  task1()
  task2()
  
  println("invoked tasks")
}