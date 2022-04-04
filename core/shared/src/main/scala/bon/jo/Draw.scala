package bon.jo
import scala.collection.mutable
import bon.jo.objects.All
import bon.jo.objects.All.Dsl.*
object Draw {

  sealed trait GridElement:
    def asGridValue[T]():GridValue[T] = this.asInstanceOf[GridValue[T]]
  object EmptyGridElement extends GridElement
  case class GridValue[T](v : T) extends GridElement
  case class GridValueExport[T](v : T,xy : Int) 

  extension [T] (v : GridValueExport[T])
    def toAllObj : All.ObjectAll[String] = obj("v" := v.v,"xy" := v.xy)
  extension [T] (v : All.ObjectAll[String] )
    def toGridValueExport :  GridValueExport[T] =GridValueExport((v / "v").value,( v / "xy").value)
  class Grid[T](val x : Int,val y : Int):
    val data : Array[GridElement] = Array((for (i <- 0 until x*y) yield EmptyGridElement ) * )

    def exportFun() : Iterable[GridValueExport[T]] = 
      data.zipWithIndex.filter(_._1 != EmptyGridElement).map((e,i) => GridValueExport(e.asGridValue[T]().v,i))
    def json() : All.ListAll[String] = All.ListAll(exportFun().map(_.toAllObj).toList)
    inline def coord(xp : Int, yp : Int) = xp * y + yp 
    def apply(xp : Int, yp : Int):GridElement = data(coord(xp,yp) )
    def update(xp : Int, yp : Int,v : GridElement) = data(coord(xp,yp)) = v
    def update[T](xp : Int, yp : Int,v : T) = data(coord(xp,yp)) = GridValue(v)


  
}
