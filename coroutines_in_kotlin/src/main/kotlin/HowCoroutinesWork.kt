class HowCoroutinesWork {
  fun first(): Int {
    return 2
  }         
  
  suspend fun second(): Int {
    return 2
  }
}

/*
kotlinc-jvm HowCoroutinesWork.kt
javap -c HowCoroutinesWork

See the difference between first and second in the bytecode
The second method takes a continuation as parameter
*/