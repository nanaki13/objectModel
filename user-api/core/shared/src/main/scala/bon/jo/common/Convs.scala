package bon.jo.common

object Convs {
  def conv:PartialFunction[Any,Long] = 
    case e : Long => e
    case e : Int => e.toLong
    case e : String => e.toLong
}
