fun compute(n: Int): Int {
  println("called...")
  return n * 2
}

val x = 4        
val temp by lazy { compute(7) }

if(x > 4 && temp > 4) {
  println("path 1")
} else {
  println("path 2")
}

println("But here...")
println(temp)
