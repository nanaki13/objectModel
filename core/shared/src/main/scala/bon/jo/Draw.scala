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
  case class Positioned[T](x : Int,y:Int,v : T)
  extension [T] (v : GridValueExport[T])
    def toAllObj : All.ObjectAll[String] = obj("v" := v.v,"xy" := v.xy)
  extension [T] (v : All.ObjectAll[String] )
    def toGridValueExport :  GridValueExport[T] =GridValueExport((v / "v").value,( v / "xy").asValue[Any]._value match 
      case e : Int => e
      case e : Long =>e.toInt
    )
  class Grid[T](val xMax : Int,val yMax : Int):
    val data : Array[GridElement] = Array((for (i <- 0 until xMax*yMax) yield EmptyGridElement ) * )

    def exportFun() : Iterable[GridValueExport[T]] = 
      data.zipWithIndex.filter(_._1 != EmptyGridElement).map((e,i) => GridValueExport(e.asGridValue[T]().v,i))
    def json() : All.ListAll[String] = All.ListAll(exportFun().map(_.toAllObj).toList)
    inline def coord(xp : Int, yp : Int) = xp * yMax + yp 
    inline def uncoord(xy : Int):(Int,Int)  = (xy / yMax, xy % yMax )
    def apply(xp : Int, yp : Int):GridElement = data(coord(xp,yp) )
    def update(xp : Int, yp : Int,v : GridElement) = data(coord(xp,yp)) = v
    def update(xp : Int, yp : Int,v : T) = data(coord(xp,yp)) = GridValue(v)
    def gridValues() = data.zipWithIndex.filter(_._1 != EmptyGridElement).map{(el,i) => 
      val (x,y) = uncoord(i)
      Positioned[String](x,y,el.asGridValue().v)  
    }
    def resetData( dataS : List[GridValueExport[T]]):Unit = 
      for (i <- 0 until xMax*yMax) {
        data(i) = EmptyGridElement
      }
      dataS.foreach{
        d => data(d.xy) = GridValue(d.v)
      }


  
}
