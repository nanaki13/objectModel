package bon.jo
import scala.collection.mutable
import bon.jo.objects.All
import bon.jo.objects.All.Dsl.*
import org.scalactic.Bool
import bon.jo.utils.ModelAndView
object Draw {

  trait Access:
    def canRead : Boolean
    def canWrite : Boolean
  trait AccessVar:
    self : Access =>
      var canRead = true
      var canWrite = true
  trait Moving[T]:
    self:Positioned[Grid[T] ] =>
      private var _moveString = ""
      private var computed : (frame : Long) => Unit = l=>()
      def moveString(s : String,xMax: Int,yMax: Int) = 
        _moveString = s
        computed = parse(s,xMax,yMax)
      def moveString : String = _moveString
      inline def move(l : Long) = computed(l)
      def parse(s : String,xMax: Int,yMax: Int) : (frame : Long) => Unit = 
       s match
          case "up" => (l) =>
            y = y-1
            if (y +  self.height) < 0 then y =  yMax
                    
          case "left"=> (l) => 
            x = x-1
            if x+ self.width < 0 then x = xMax
          case "down"=> (l) => 
            y = y+1
            if y > yMax then y = -self.height
          case "right"=> 
            (l) => x = x+1
            if x > xMax then x = -self.width
          case _ => e => ()



  extension [T]  (e : Positioned[Grid[T] ] )
    def width : Int = e.v.xSize
    def height : Int = e.v.ySize
    inline def coorInMe(xp : Int, yp : Int) : (Int,Int) = (xp - e.x,yp - e.y)
    def isIn(xp : Int, yp : Int) : Boolean = 
      val (xx,yy) = coorInMe(xp,yp)
      e.v.isInGrid(xx,yy)
  extension [T] (e : Positioned[Grid[T] ] with Access)
    def canReadAndIsIn(xp : Int, yp : Int) : Boolean = e.canRead && e.isIn(xp,yp)
    def canWriteAndIsIn(xp : Int, yp : Int) : Boolean = e.canWrite && e.isIn(xp,yp)
  type SheetMV[T,V] = ModelAndView[Positioned[Grid[T]],V] 

  class FrameGrid[T](override val xSize : Int,override val ySize : Int) extends Grid[T]( xSize : Int, ySize : Int):
    var frames : Seq[Array[GridElement[T]]] = IndexedSeq(data)
    var currentFrame = 0
    override def apply(xp : Int, yp : Int) : GridElement[T] = frames(currentFrame)(coord(xp,yp))
    override def  update(xp : Int, yp : Int, e : GridElement[T]):Unit =  frames(currentFrame)(coord(xp,yp)) = e

    def nextFrame() = 
      println(currentFrame)
      currentFrame = (currentFrame + 1) % frames.size
    
    def addFrame() = frames = frames :+ emptyData()
  class MasterGrid[T](override val xSize : Int,override val ySize : Int) extends Grid[T]( xSize : Int, ySize : Int):
    override def json() : All.ObjectAll[String] = 
      var base = super.json()
      base + obj("sheets" := All.ListAll[String](sheet.map(v => obj( "x" := v.x, "y":= v.y) + v.v.json())))
    var sheet : List[Positioned[Grid[T]] with Access with AccessVar with Moving[T]] = Nil
    override def apply(xp : Int, yp : Int) : GridElement[T] = 
      sheet.filter(_.canReadAndIsIn(xp,yp)).flatMap{
        p => 

          val (xx,yy) = p.coorInMe(xp,yp)
          p.v(xx,yy) match
            case EmptyGridElement => None
            case o => Some(o)
      }.headOption.getOrElse(super.apply(xp,yp))
    override def  update(xp : Int, yp : Int, e : GridElement[T]):Unit = 
      sheet.find(_.canWriteAndIsIn(xp,yp)).map{
        p => 
          val (xx,yy) = p.coorInMe(xp,yp)
          p.v(xx,yy) = e
      }.getOrElse(super.update(xp,yp, e))
   
  sealed trait GridElement[+T]:
    def asGridValue[B >: T]():GridValue[B] = this.asInstanceOf[GridValue[B]]
  object EmptyGridElement extends GridElement[Nothing]
  case class GridValue[T](v : T) extends GridElement[T]
  case class GridValueExport[T](v : T,xy : Int) 

  case class Positioned[T](var x : Int,var y:Int,var v : T)
  extension [T] (v : GridValueExport[T])
    def toAllObj : All.ObjectAll[String] = obj("v" := v.v,"xy" := v.xy)
  extension [T] (v : All.ObjectAll[String] )
    def toGridValueExport :  GridValueExport[T] =GridValueExport((v / "v").value,( v / "xy").asValue[Any]._value match 
      case e : Int => e
      case e : Long =>e.toInt
    )
  trait NormalAccessGrid[T]:
    self : Grid[T] =>
      def apply(xp : Int, yp : Int):GridElement[T] = data(coord(xp,yp) )
      def update(xp : Int, yp : Int,v : GridElement[T]) :Unit= data(coord(xp,yp)) = v
      inline def update(xp : Int, yp : Int,v : T):Unit = this(xp, yp) = GridValue(v)

  class Grid[T](val xSize : Int,val ySize : Int) extends GridOps[T] with  NormalAccessGrid[T]
  trait GridOps[T]:
    val xSize : Int
    val ySize : Int
    val data : Array[GridElement[T]] =  emptyData()
    def emptyData(): Array[GridElement[T]] =  Array((for (i <- 0 until xSize*ySize) yield EmptyGridElement ) * )
    def isInGrid(xp : Int, yp : Int):Boolean = isInGridX(xp) && isInGridY(yp)
    inline def isInGridX(xp : Int):Boolean = xp > -1 && xp < xSize
    inline def isInGridY( yp : Int):Boolean = yp > -1 && yp < ySize
    def exportFun() : Iterable[GridValueExport[T]] = 
      data.zipWithIndex.filter(_._1 != EmptyGridElement).map((e,i) => GridValueExport(e.asGridValue[T]().v,i))
    def json() : All.ObjectAll[String] = 
      obj("xSize" := xSize,"ySize" := ySize,"data" := All.ListAll(exportFun().map(_.toAllObj).toList))
      
    inline def coord(xp : Int, yp : Int) = xp * ySize + yp 
    inline def uncoord(xy : Int):(Int,Int)  = (xy / ySize, xy % ySize )
    def apply(xp : Int, yp : Int):GridElement[T]
    def update(xp : Int, yp : Int,v : GridElement[T]) :Unit
    inline def update(xp : Int, yp : Int,v : T):Unit
    def gridValues() = data.zipWithIndex.flatMap{(el,i) => 
      val (x,y) = uncoord(i)
      val ell = this(x,y)
      ell match
        case el : GridValue[_] => Option( Positioned[T](x,y,el.asGridValue().v)  )
        case o => None
    }
     
    def resetData( dataS : List[GridValueExport[T]]):Unit = 
      for (i <- 0 until xSize*ySize) {
        data(i) = EmptyGridElement
      }
      dataS.foreach{
        d => data(d.xy) = GridValue(d.v)
      }


  
}
