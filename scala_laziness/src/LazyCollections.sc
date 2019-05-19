def isPrime(number: Int) = {
  number > 1 && !(2 to number - 1).exists { i => number % i == 0 }  
}

def prime(starting: Int) : Stream[Int] = {
  if(isPrime(starting))
    starting #:: prime(starting + 1)
  else
    prime(starting + 1)
}

println(prime(10).take(10).toList)