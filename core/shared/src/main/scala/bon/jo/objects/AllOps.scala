package bon.jo.objects
import bon.jo.objects.All
import bon.jo.objects.All.ObjectAll
import bon.jo.objects.All.Empty
import bon.jo.objects.All.Path
import bon.jo.objects.All.ObjectProp
/**
 * Operation for [[All]]
*/
trait AllOps[Key]:
  self : All[Key] =>

  /**
   * shorthand of `apply`
  * returns value of key in this.
  * Nothing is done if newK doen't exists or this is not an [[All.ObjectAll]] 
  */ 
  inline def /(key : Key):All[Key] = apply(key)

  /**
  * Returns if this contains key.
  * Nothing is done if newK doen't exists or this is not an [[All.ObjectAll]] 
  */
  def contains(key : Key): Boolean = 
    this match
      case o : ObjectAll[Key] => o.props.exists(_.key == key)
      case _ => false 

  /**
   * Returns copy with rename `old` Key in `newK`, keep the value.
   * Nothing is done if `newK` doen't exists or this is not an [[All.ObjectAll]]  
  */
  def replace(newK : Key, old : Key):All[Key] = 
    this match 
      case o : ObjectAll[Key] => 
        o.copy(o.props.map{
          case e if e.key == old => e.copy(key = newK)
          case o => o
        })
      case _ => this
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
  def update(path  : Path[Key], value : All[Key]) :All[Key] = 
      if path.values.isEmpty then value
      else if path.values.size == 1 then update(path.values.head,value)
      else
        this match {
        case o : ObjectAll[Key] => 
          o.props.find(_.key ==path.values.head).map{
            prop => 
              val updated = prop.value.update(Path(path.values.tail),value)
              o.copy(o.props.filter(_.key != path.values.head):+ All.ObjectProp(path.values.head, updated))
          }.getOrElse(o)
        case _ => this
    }
  /**
   * Returns copy with updated  value for key.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update(key  : Key, value : All[Key]) :All[Key] = 
    this match {
      case o : ObjectAll[Key] => o.copy(o.props.filter(_.key != key) :+ ObjectProp(key,value))
      case _ => this
    }

  /**
  * 
  * returns key's value in this.
  * Nothing if this is not an [[All.ObjectAll]] 
  */
  def apply(key : Key) : All[Key] =

    this match {
      case ObjectAll(props) => props.find(_.key == key).get.value
      case _ => Empty[Key]()
    }

  /**
  * 
  * returns path's value in this.
  * Nothing if this is not an [[All.ObjectAll]] 
  */
  def apply(s : Path[Key]) : All[Key] =
      if s.values.isEmpty then this
      else if s.values.size == 1 then /(s.values.head)
      else if s.values.isEmpty then All.Empty()
      else apply(s.values.head).apply(Path(s.values.tail))

