package bon.jo.sql

class Alias:
  import scala.collection.mutable
  val givenAlias: mutable.Map[String, Int] = mutable.Map.empty
  def apply(s: String): String =
    val pref = givenAlias.getOrElseUpdate(s, 1)
    givenAlias += s -> (pref + 1)
    s + "_" + (pref)
object Alias:
  inline def alias: Alias ?=> Alias = summon
