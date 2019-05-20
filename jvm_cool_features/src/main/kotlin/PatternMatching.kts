fun process(input: Any) {
  when(input) {
    1 -> println("It's a one")
    in 13..19 -> println("a teen")
    is String -> println("It's a string")
    else -> println("no clue")
  }
}

process(1)
process(15)
process("Hello")
process(3.2)
