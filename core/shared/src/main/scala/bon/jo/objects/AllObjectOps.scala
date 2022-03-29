package bon.jo.objects
import bon.jo.objects.All
import bon.jo.objects.All.ObjectAll
import bon.jo.objects.All.Empty
import bon.jo.objects.All.Path
import bon.jo.objects.All.ObjectProp
/**
 * Operation for [[All]]
*/
trait AllObjectOps[Key]:
  self : All.ObjectAll[Key] =>

  def +(o :  All.ObjectAll[Key]) = copy(props ++ o.props)


  /**
  * Returns if this contains key.
  * Nothing is done if newK doen't exists or this is not an [[All.ObjectAll]] 
  */
  override def contains(key : Key): Boolean = props.exists(_.key == key)

  /**
   * Returns copy with rename `old` Key in `newK`, keep the value.
   * Nothing is done if `newK` doen't exists or this is not an [[All.ObjectAll]]  
  */
  override def replace(newK : Key, old : Key):All[Key] = copy(props.map{
          case e if e.key == old => e.copy(key = newK)
          case o => o
        })
  
  /**
   * Returns copy with updated  value for path.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update(path  : Path[Key], value : All[Key]) :All[Key] = 
      if path.values.isEmpty then value
      else if path.values.size == 1 then update(path.values.head,value)
      else
        props.find(_.key ==path.values.head).map{
            prop => 
              val updated = prop.value.update(Path(path.values.tail),value)
              copy(props.filter(_.key != path.values.head):+ All.ObjectProp(path.values.head, updated))
          }.getOrElse(this)
    
  /**
   * Returns copy with updated  value for key.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  def update(key  : Key, value : All[Key]) :All[Key] = copy(props.filter(_.key != key) :+ ObjectProp(key,value))

  /**
  * 
  * returns key's value in this.
  * Nothing if this is not an [[All.ObjectAll]] 
  */
  def apply(key : Key) : All[Key] = props.find(_.key == key).get.value

  def get(key : Key) : Option[All[Key]] = props.find(_.key == key).map(_.value)

  def delete(k: Key): All[Key] = copy(props.filter(_.key != k))



