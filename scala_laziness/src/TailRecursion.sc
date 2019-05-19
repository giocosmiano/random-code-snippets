import scala.util.control.TailCalls._

def producer(number: Int) : TailRec[Int] = {
  if(number < 100000) {
    val next = number + 1
    tailcall(consumer(next))
  } else {
    done(number)
  }
}

def consumer(number: Int) : TailRec[Int] = {
  println(s"consumed $number")
  producer(number)
}

producer(1).result