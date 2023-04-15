package bon.jo.common

object FunBuilder :



  trait Factory[T]:
    def apply() : T
  
  case class Building[T](var onBuild : T)
  def building[T] (using Factory[T]):Building[T] = Building(summon())
  def build[T](fs: Building[T] ?=> Unit *):Factory[T] ?=> T = 
    given Building[T] = building
    fs.foreach(f => f)
    onBuild
  def build[T](v : T)(fs: Building[T] ?=> Unit *):T = 
    given Building[T] = Building(v)
    fs.foreach(f => f)
    onBuild
  inline def onBuild[T] : Building[T] ?=> T = summon.onBuild
  inline def $[T] : Building[T] ?=> T = onBuild
  inline def |[T](fs: Building[T] ?=> Unit *):Factory[T] ?=> T = build(fs *)
