package bon.jo
object MainJvm {
  case class Point(x: Int, y:Int) extends SystemElement
  case class Univers(elements : List[SystemElement]) extends System
  class ProcessEl(using List[SystemElement] => Univers) extends SystemElementProcess:
    type T = Point
    def next(s: Point,onBuild: System): SystemFlow = 
      onBuild.update(s,s.copy(x = s.x + 1))


  @main
  def test2():Unit =
    val u : Univers = Univers(List(Point(1,2),Point(7,2)))
    given (List[SystemElement] => Univers) = Univers(_)
    given ProcessEl  = ProcessEl()
    val ns = u.nextSystem()
    println(ns)
    println(ns.nextSystem())

}
