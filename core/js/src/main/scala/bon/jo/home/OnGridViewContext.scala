package bon.jo.home
import bon.jo.home.GridViewContext
import bon.jo.home.GridViewContext.*
import org.scalajs.dom.TouchEvent
import org.scalajs.dom.HTMLElement
import bon.jo.home.ProcessEvent.SelectActionParam
import bon.jo.Draw.Positioned
import bon.jo.Draw.GridValueExport
import org.scalajs.dom.HTMLCanvasElement
import bon.jo.Draw.GridValue
import org.scalajs.dom.MouseEvent
import bon.jo.Draw.Grid


trait OnGridViewContext {
  def xyInGrid( ev: MouseEvent): OnContext[(Int,Int)] = ((xme(ev) / context.factor.toDouble).toInt, (yme(ev) / context.factor.toDouble).toInt)
  def xyInGrid( ev: TouchEvent): OnContext[(Int,Int)] = 
    val (xx,yy) = xy(ev)
    (((xx/context.factor.toDouble).toInt),(yy/context.factor.toDouble).toInt)

  def xInGrid(x: Int): OnContext[Int] = (x / context.factor.toDouble).toInt
  def yInGrid(y: Int): OnContext[Int] = (y / context.factor.toDouble).toInt
  def xme( ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetX.asInstanceOf[Int]
  def yme( ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetY.asInstanceOf[Int]
  def xy( ev: TouchEvent): (Int,Int) =
    val rect = ev.target.asInstanceOf[HTMLElement].getBoundingClientRect()
    val t  = if !scalajs.js.isUndefined( ev.targetTouches(0)) then  ev.targetTouches(0) else ev.changedTouches(0)
  
    ((t.pageX - rect.left).toFloat.round,(t.pageY - rect.top).toFloat.round)

 

  def draw(xI : Int,yI : Int): OnContextUnit =
    given HTMLElement = context.canvas
    val fact = context.factor
    val grid = context.grid
    val colot = context.color
   
    val y_ = yInGrid
    grid(xI, yI) = GridValue(colot.toString)
    drawPoint(xI, yI, colot.toString)

  def prepare(c: HTMLCanvasElement, cWidth: Int, cHeight: Int): OnContextUnit =
    c.width = cWidth
    c.height = cHeight
    c.style.backgroundColor = "white"
  def drawPoint(xGrid: Int, yGrid: Int, color: String): OnContextUnit =
    val g = context.gc
    val fact = context.factor
    g.beginPath()
    g.fillStyle = color
    g.rect(xGrid * fact, yGrid * fact, fact, fact)
    g.fill()
    g.closePath()
  def updateGridAndDraw(xx : Int,yy : Int) :OnContextUnit= 
    context.grid(xx,yy) = context.color.toString
    drawPoint(xx,yy,context.color.toString)
  

  def resetData(dataS: List[GridValueExport[String]],xSize : Int, ySize : Int): OnContextUnit =
    context.grid = Grid(xSize,ySize)
    context.grid.resetData(dataS)
    context.canvas.width = (xSize * context.factor.toFloat).round
    context.canvas.height = (ySize * context.factor.toFloat).round
    draw()
  def draw(): OnContextUnit =
    context.gc.clearRect(0, 0, context.canvas.width, context.canvas.height)
    context.grid.gridValues().foreach { case Positioned(x, y, color) =>
      drawPoint(x, y, color)

  }
  def updateGrid(xMaxx : Int,yMax : Int): OnContextUnit =
    val nGrid = Grid[String](xMaxx,yMax)
    for{
      
      xx <- 0 until xMaxx
      yy <- 0 until yMax
    } {
      if context.grid.isInGrid(xx,yy) then
        nGrid(xx,yy) =  context.grid(xx,yy)
      

    }
    context.grid = nGrid
    context.canvas.width = (xMaxx * context.factor.toFloat).round
    context.canvas.height = (yMax * context.factor.toFloat).round
    draw()
  def updateFacor(pxSize : Int):OnContextUnit = 
    context.factor = pxSize
    context.canvas.width = (context.grid.xSize * context.factor.toFloat).round
    context.canvas.height = (context.grid.ySize  * context.factor.toFloat).round
    draw()
}