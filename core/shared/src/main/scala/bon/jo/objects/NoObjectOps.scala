package bon.jo.objects
import bon.jo.objects.All
import bon.jo.objects.All.ObjectAll
import bon.jo.objects.All.Empty
import bon.jo.objects.All.Path
import bon.jo.objects.All.ObjectProp
/**
 * Operation for [[All]]
*/
trait NoObjectOps[Key]:
  self : All[Key] =>



  /**
  * Returns if this contains key.
  * Nothing is done if newK doen't exists or this is not an [[All.ObjectAll]] 
  */
  override def contains(key : Key): Boolean = false

  /**
   * Returns copy with rename `old` Key in `newK`, keep the value.
   * Nothing is done if `newK` doen't exists or this is not an [[All.ObjectAll]]  
  */
  override def replace(newK : Key, old : Key):All[Key] = this
  /**
   * Returns copy with  empty object for key.
   * Nothing is done if `newK` doen't exists or this is not an [[All.ObjectAll]]  
  */
  override def empty(key  : Key):All[Key] = this
  /**
   * Returns copy with  empty object for key.
   * Nothing if this is not an [[All.ObjectAll]]  
  */
  override def empty(key  : Path[Key]):All[Key] = this
  
  /**
   * Returns copy with updated  value for path.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  override def update(path  : Path[Key], value : All[Key]) :All[Key] = this
  /**
   * Returns copy with updated  value for key.
   * Nothing if this is not an [[All.ObjectAll]] 
  */
  override def update(key  : Key, value : All[Key]) :All[Key] = this

  /**
  * 
  * returns key's value in this.
  * Nothing if this is not an [[All.ObjectAll]] 
  */
  override def apply(key : Key) : All[Key] =Empty[Key]()

  override def delete(key : Key): All[Key] = this



