package bon.jo

object Validator:
  case class ValidatorException[T](value : T,message : String) extends RuntimeException(s"value : $value not valid : \n, $message")
  enum ValidationResult[T](value : T):
    case Success(value : T) extends ValidationResult(value)
    case Failure(value : T,message : String) extends  ValidationResult(value)
  extension [T](b : ValidationResult[T])
    def and(c: ValidationResult[T]) : ValidationResult[T] = 
      (b,c) match
        case (ValidationResult.Success(v),ValidationResult.Success(_)) => ValidationResult.Success(v)
        case (ValidationResult.Failure(v,message),ValidationResult.Success(_)) => ValidationResult.Failure(v,message)
        case (ValidationResult.Success(v),ValidationResult.Failure(_,message)) => ValidationResult.Failure(v,message)
        case (ValidationResult.Failure(v,message1),ValidationResult.Failure(_,message2)) => ValidationResult.Failure(v,message1+message2)
      
  def apply[T](f : T => ValidationResult[T]) : Validator[T] = 
    t => 
      f(t) match
        case ValidationResult.Success(value )=>value
        case ValidationResult.Failure(value,message)=> throw ValidatorException(value,message)
      
  extension [T](t : T)
    def validate() : Validator[T] ?=> T = summon.validate(t)
trait Validator[T]:
  def validate(t : T) : T
