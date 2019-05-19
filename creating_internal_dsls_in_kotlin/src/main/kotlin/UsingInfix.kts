class Car {
  infix fun drive(dist: Int) {
    println("driving... $dist")
  }
}

val car = Car()

car.drive(10)

car drive 10