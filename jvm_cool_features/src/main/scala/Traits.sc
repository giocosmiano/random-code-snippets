trait Friend {
  val name: String //emphasizes that class that mixes this in should have a name
  def listen = println(s"I'm $name, listening")  
}

class Human(val name: String) extends Friend

def seekHelpFrom(friend: Friend) = friend.listen

val peter = new Human("Peter")

seekHelpFrom(peter)

class Animal(val name: String)
class Dog(override val name: String) extends Animal(name) with Friend

val buddy = new Dog("Buddy")
seekHelpFrom(buddy)

class Cat(override val name: String) extends Animal(name)

val alf = new Cat("Alf")

//seekHelpFrom(alf) //ERROR

val yourSweetCat = new Cat("Carfield") with Friend

seekHelpFrom(yourSweetCat)