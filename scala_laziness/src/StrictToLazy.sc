import scala.language.postfixOps

val people = List(
  ("Joe", 2),
  ("Paula", 15),
  ("Jim", 22),
  ("Jane", 42),
  ("Joe", 72),
  ("Kim", 2),
  ("Bill", 52),
  ("Jill", 32))

def isOlderThan30(person: (String, Int)) = {
  println(s"isOlderThan30 for $person")
  person._2 > 30
}

def nameIs4LettersLong(person: (String, Int)) = {
  println(s"nameIs4LettersLong for $person")
  person._1.length == 4
}

println(people.toStream filter isOlderThan30 filter nameIs4LettersLong head)

//Only change people to people.view
//Compare the output between this and version in 6_strict_collections