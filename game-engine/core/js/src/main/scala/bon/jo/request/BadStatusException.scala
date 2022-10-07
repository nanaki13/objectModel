package bon.jo.request

import scala.reflect.ClassTag

case class BadStatusException[KO](value : KO) extends RuntimeException:
    def unapply[T](bad : BadStatusException[_] )(implicit ev: ClassTag[T]):Option[T] = bad.value match
        case t : T => Some(t)
        case o => None
class BadStatusExceptionValue[T](implicit ev: ClassTag[T]):
    def unapply(bad : BadStatusException[_] ):Option[T] = bad.value match
        case t : T => Some(t)
        case o => None
    
