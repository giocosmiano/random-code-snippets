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

val useDefault = Math.random() > 0.50
val message = if (useDefault) "hello world,\n\t Hello There!!!" else null
println(wordCount(message))

