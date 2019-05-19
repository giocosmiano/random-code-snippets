import scala.io._

val first = StdIn.readInt
val second = StdIn.readInt

if(Math.random() > 0.5)
  second
  
println(first - second)  

lazy val firstAgain = StdIn.readInt
lazy val secondAgain = StdIn.readInt

if(Math.random() > 0.5)
  secondAgain
  
println(firstAgain - secondAgain)  

//Enter
//1
//2
//1
//2
//Try running the program several times with the same inputs