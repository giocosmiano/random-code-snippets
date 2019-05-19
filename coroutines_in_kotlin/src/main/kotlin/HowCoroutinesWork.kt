class Sample {
  fun first(): Int {
    return 2
  }         
  
  suspend fun second(): Int {
    return 2
  }
}

/*
kotlinc-jvm Sample.kt
javap -c Sample      

See the difference between first and second in the bytecode
The second method takes a continuation as parameter
*/