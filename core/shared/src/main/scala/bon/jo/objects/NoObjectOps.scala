package bon.jo.objects
import bon.jo.objects.All
import bon.jo.objects.All.ObjectAll
import bon.jo.objects.All.Empty
import bon.jo.objects.All.Path
import bon.jo.objects.All.ObjectProp
/**
 * Operation for [[All]] that are not AllObject.
*/
trait NoObjectOps[Key]:
  self : All[Key] =>


  override def contains(key : Key): Boolean = false

  override def replace(newK : Key, old : Key):All[Key] = this

  override def empty(key  : Key):All[Key] = this

  override def empty(key  : Path[Key]):All[Key] = this
  
  override def update(path  : Path[Key], value : All[Key]) :All[Key] = this

  override def update(key  : Key, value : All[Key]) :All[Key] = this

  override def apply(key : Key) : All[Key] =Empty[Key]()

  override def delete(key : Key): All[Key] = this



