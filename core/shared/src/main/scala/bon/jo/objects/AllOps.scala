package bon.jo.objects
import bon.jo.objects.All
import bon.jo.objects.All.ObjectAll
import bon.jo.objects.All.Empty
import bon.jo.objects.All.Path
import bon.jo.objects.All.ObjectProp
/**
 * Operations for [[All]]
*/
trait AllOps[Key]:

  self : All[Key] =>
  /**
   * `apply` shortand
  * returns value of key in this.
  * Nothing is done if newK doen't exists or this is not an [[All.ObjectAll]] 
  */ 
  inline def /(key : Key):All[Key] = apply(key)
  /**
   * `delete` shortand 
  */
  inline def -(key : Key):All[Key] = delete(key)
  /**
   * `delete` shortand 
  */
  inline def -(key : Path[Key]):All[Key] = delete(key)

  /**
  * Returns if this contains key.
  * Nothing is done if newK doen't exists or this is not an [[All.ObjectAll]] 
  */
  def contains(key : Key): Boolean 

  /**
   * Returns copy with rename `old` Key in `newK`, keep the value.
   * Nothing is done if `newK` doen't exists or this is not an [[All.ObjectAll]]  
  */
  def replace(newK : Key, old : Key):All[Key]
  /**
   * Returns copy with  empty object for key.
   * Nothing is done if `newK` doen't exists or this is not an [[All.ObjectAll]]  
  */
  def empty(key  : Key):All[Key] = update(key,Empty())
  /**
   * Returns copy with  empty object for key.
   * Nothing if this is not an [[All.ObjectAll]]  
  */
  def empty(key  : Path[Key]):All[Key] = 
    update(key,Empty())
  
  /**
   * Returns copy with updated  value for path.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update(path  : Path[Key], value : All[Key]) :All[Key] 
  /**
   * Returns copy with updated  value for key.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update(key  : Key, value : All[Key]) :All[Key]

  /**
   * Returns copy with updated  value for key.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update[V](key  : Key, value : V) :All[Key] = update(key,All.Value(value))

  /**
   * Returns copy with updated  value for key.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update[V](path  : Path[Key], value : V) :All[Key] = update(path,All.Value(value))

  /**
   * Returns copy without key 'k'.
  */
  def delete(k : Key) : All[Key]
  /**
   * Returns copy where object propertie of this with path `p` is replaced  by same without `p.last` propertie.
  */
  def delete(p : Path[Key]) : All[Key]=
    p match
      case Path(List(end)) => delete(end)
      case Path(head :: tail) => update(head,(this / head) - (Path(tail)))
      case o => this


  /**
  * 
  * Returns key's value in this.
  * Nothing if this is not an [[All.ObjectAll]] 
  */
  def apply(key : Key) : All[Key]

  /**
   * Returns if the key exists in this.
   * 
  */
  def isEmpty(key : Key) : Boolean = contains(key) && this / key == Empty[Key]()

  /**
  * 
  * Returns path's value in this.
  * Nothing if this is not an [[All.ObjectAll]] 
  */
  def apply(s : Path[Key]) : All[Key] =
      if s.values.isEmpty then this
      else if s.values.size == 1 then /(s.values.head)
      else if s.values.isEmpty then All.Empty()
      else apply(s.values.head).apply(Path(s.values.tail))

  /**
   * Returns cast this in [[All.Value]].
  */
  inline def asValue[T] : All.Value[Key,T] = asInstanceOf
  /**
   * Return value of this casted in [[All.Value]]. 
  */
  inline def value[T] : T = asValue._value
  /**
   * Returns cast this in [[All.ObjectAll]].
  */
  inline def asObject : All.ObjectAll[Key] = asInstanceOf
  /**
   * Returns cast this in [[All.ListAll]].
  */
  inline def asList : All.ListAll[Key] = asInstanceOf

