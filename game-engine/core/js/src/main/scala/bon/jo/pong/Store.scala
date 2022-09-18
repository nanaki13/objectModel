package bon.jo.pong
import org.scalajs.dom.window
import scalajs.js.isUndefined
object Store:
  
  trait Ser[T]:
    def toString(t : T) :String
    def fromString(t : String) :T
  given Ser[String] with
    def toString(t : String) :String = t
    def fromString(t : String) :String = t
  type StringMaker[T,A] = Ser[T]?=> A
  type String_[T] = StringMaker[T,String]
  type Self[T] =  StringMaker[T,Ser[T]]
  type Unser[T] = StringMaker[T,T]
  type UnserOption[T] = StringMaker[T,Option[T]]
  type UStringMaker[T] = Ser[T]?=> Unit
  def ser[T] : Self[T] = summon
  extension (s: String)
    def to[T]:Unser[T] = ser.fromString(s)
    def storageRead[T]:UnserOption[T] =
      if s.storageExists then Some(get(s)) else None
    def storageExists : Boolean = have(s)
    def storageRemove: Unit = store.removeItem(s)
 
  extension [T] (s: T)
    def makeString:String_[T] = ser.toString(s)
    def storageWrite(key : String): UStringMaker[T] = save(key,s)
 
  private val store = window.localStorage
  private def _save(key : String,value : String):Unit = store.setItem(key,value)
  private def _get(key : String):String = store.getItem(key)
  def have(key : String) : Boolean = 
    val item = store.getItem(key)
    item != null && ! isUndefined(item)
  def save[T](key : String,value : T):UStringMaker[T] = _save(key,value.makeString)
  def get[T](key : String): Unser[T]  = _get(key).to[T]
  
