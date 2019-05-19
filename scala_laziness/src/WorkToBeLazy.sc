def greetKid() = println("Hi kiddo")
def greetAdult() = println("Hi there")

def greet(age: Int, kidGreet: () => Unit, adultGreet: () => Unit) = {
  print(s"$age years:")
  if(age < 18) kidGreet() else adultGreet()
}

greetKid
greetAdult
greet(12, greetKid, greetAdult)
greet(22, greetKid, greetAdult)