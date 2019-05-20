class Car {
  infix fun turn(direction: String): Car {
    println("turn $direction")
    return this
  }
}

val car = Car()

car turn "left" turn "right"