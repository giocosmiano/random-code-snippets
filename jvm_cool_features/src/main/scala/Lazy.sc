def compute(n: Int) = {
  println("called...")
  n * 2
}

val x = 4        
lazy val temp = compute(7)

if(x > 4 && temp > 4) {
  println("path 1")
} else {
  println("path 2")
}

println("But here...")
println(temp)
