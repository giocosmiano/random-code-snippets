import scala.io._

def expensiveComputation() = {
  println("expensive")
  42  
}

lazy val result = expensiveComputation() //try running with lazy, then removing lazy



print("Enter 0 or 10:")
val input = StdIn.readInt

if(input > 5)
  println(result)
else
  println("no result")


//With lazy, the expensive computation is run only if needed.