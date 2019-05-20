import java.math.*

fun factorial(n: Int): BigInteger = 
  if(n == 1) BigInteger("1") else BigInteger(n.toString()) * factorial(n - 1)
  
println(factorial(5))  
//println(factorial(50000))

tailrec fun factorialTCO(n: Int, fact: BigInteger = BigInteger("1")): BigInteger =
  if(n == 1) fact else factorialTCO(n - 1, fact * BigInteger(n.toString()))
  
println(factorialTCO(5))
println(factorialTCO(50000))
