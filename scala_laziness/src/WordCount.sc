
def wordCount(sentence: String): Unit = {
  Option(sentence)
    .map(e => e.toLowerCase)
    .map(e => e.split(" "))
    .toList
    .flatten
    .map(e => e.replaceAll("\\W", ""))
    .groupBy(identity)
    .map(e => e._1 -> e._2.size)
    .toSeq
    .sortBy(_._1)
    .foreach(e => println(e))
}

wordCount("hello world, Hello There!!!")
