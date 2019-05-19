import kotlinx.coroutines.*

val sequence = sequence {
  println("one")
  yield(1)

  println("two")
  yield(2)

  println("three")
  yield(3)
}

for(value in sequence) {
  println(value)
}