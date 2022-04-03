package bon.jo
import scala.collection.mutable
object Draw {
  sealed trait GridElement
  object EmptyGridElement extends GridElement
  case class GridValue[T](v : T) extends GridElement

  class Grid[T](val x : Int,val y : Int):
    val data : Array[GridElement] = Array((for (i <- 0 until x*y) yield EmptyGridElement ) * )
    inline def coord(xp : Int, yp : Int) = xp * y + yp 
    def apply(xp : Int, yp : Int):GridElement = data(xp * y + yp )
    def update(xp : Int, yp : Int,v : GridElement) = data(coord(x,y)) = v
    def update[T](xp : Int, yp : Int,v : T) = data(coord(x,y)) = GridValue(v)


  def test = 
    val g = Grid[String](10,10)
    g(5,5) = EmptyGridElement
    g(5,5) = "test"
}
