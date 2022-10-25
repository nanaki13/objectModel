package bon.jo.html.request

import scala.reflect.ClassTag

case class BadStatusException[KO](value : KO) extends RuntimeException
class BadStatusExceptionValue[T](using ClassTag[T]):
    def unapply(bad : BadStatusException[_] ):Option[T] = bad.value match
        case t : T => Some(t)
        case o => None
    
