def printValuesInSequence(starting: Int, count: Int, generator: Int => Int) = {
  var result = starting
  for(i <- 1 to count) {
    result = generator(result)
    println(result)
  }
}

printValuesInSequence(10, 10, value => value + 1)

println("----")

printValuesInSequence(10, 10, value => value + 2)