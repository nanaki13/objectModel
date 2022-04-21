package bon.jo.home

case class VarValue[T](var value : T)
extension [T](v : VarValue[Option[T]])
  def get(ifNot : =>T):T =
    v.value match 
      case None => 
        v.value = Some(ifNot)
      case _ =>
    v.value.get

type VarValueDouble = VarValue[Double]