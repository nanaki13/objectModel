package bon.jo
import scala.reflect.ClassTag
package service:



  enum SaveResult:
    case Updated
    case NotUpdated
    case Confilct
    case OK[T](value : T)
  object SaveResultEx:
    class OK[T](using ClassTag[T]):
      def unapply(s : SaveResult) : Option[T] =
        s match
          case SaveResult.OK(value ) => 
            value match
              case e : T => Some(e)
          case o => None
            
 
  object SaveResultSuccess:
    def unapply(c : SaveResult) : Option[SaveResult] = 
      c match
        case SaveResult.Updated => Some(SaveResult.Updated)
        case SaveResult.NotUpdated => None
        case SaveResult.Confilct => None
        case SaveResult.OK(value) => Some(SaveResult.OK(value))
        