import scala.collection.breakOut
import scala.collection.immutable.TreeMap

// using `breakout` to transform `map` to `treeMap`
// https://docs.scala-lang.org/tutorials/FAQ/breakout.html
def wordCount(sentence: String): TreeMap[String, Int] = {
  Option(sentence)
    .orElse(Some(""))
    .map(e => e.toLowerCase)
    .map(e => e.split("\\s"))
    .toList
    .flatten
    .filter(_.trim.nonEmpty)
    .map(e => e.replaceAll("\\W", ""))
    .groupBy(identity)
    .map(e => e._1 -> e._2.size)(breakOut)
//    .toSeq  // no need as it returns a TreeMap sorted by keys
//    .sortBy(_._1)
//    .toMap
}

val text = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
println(wordCount(text))
