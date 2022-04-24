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
import bon.jo.Draw.MasterGrid
import bon.jo.Draw.Access
import bon.jo.Draw.AccessVar
import bon.jo.Draw.Moving
import bon.jo.Draw.EmptyGridElement


trait OnGridViewContext {
  def xyInGrid( ev: MouseEvent): OnFactor[(Int,Int)] = ((xme(ev) / factor.toDouble).toInt, (yme(ev) / factor.toDouble).toInt)
  def xyInGrid( ev: TouchEvent): OnFactor[(Int,Int)] = 
    val (xx,yy) = xy(ev)
    (((xx/factor.toDouble).toInt),(yy/factor.toDouble).toInt)

  def xInGrid(x: Int): OnFactor[Int] = (x / factor.toDouble).toInt
  def yInGrid(y: Int): OnFactor[Int] = (y / factor.toDouble).toInt
  def xme( ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetX.asInstanceOf[Int]
  def yme( ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetY.asInstanceOf[Int]
  def xy( ev: TouchEvent): (Int,Int) =
    val rect = ev.target.asInstanceOf[HTMLElement].getBoundingClientRect()
    val t  = if !scalajs.js.isUndefined( ev.targetTouches(0)) then  ev.targetTouches(0) else ev.changedTouches(0)
  
    ((t.pageX - rect.left).toFloat.round,(t.pageY - rect.top).toFloat.round)

 
  def delete(xI : Int,yI : Int): OnBaseDrawFactor[Unit] =
    grid(xI, yI) = EmptyGridElement
    drawDeletePoint(xI, yI)
  def draw(xI : Int,yI : Int): OnContextUnit =
    val grid = context.grid
    val colot = context.color
    context.actionParam match
      case DrawParam.Pixel => 
          grid(xI, yI) = GridValue(colot.toString)
          drawPoint(xI, yI, colot.toString)
      case  DrawParam.Circle(r) => 
        val data = Grid[String](2*r+1,2*r+1)
        data.circle(colot.toString,r)
      
        val roundR = r
        for{
          xr <- 0 until 2*r+1
          yr <- 0 until 2*r+1
          dataP = data(xr,yr) 
          xIn = xI - roundR + xr
          yIn = yI - roundR + yr
        } {
          if dataP != EmptyGridElement && grid.isInGrid(xIn, yIn) then
            grid(xIn, yIn) = dataP
            drawPoint(xIn, yIn, colot.toString)  
        }
       
   

  def prepare(c: HTMLCanvasElement, cWidth: Int, cHeight: Int): OnContextUnit =
    c.width = cWidth
    c.height = cHeight
    c.style.backgroundColor = "white"
  def drawPoint(xGrid: Int, yGrid: Int, color: String):OnBaseDrawFactor[Unit] =
    val g = gc
    val fact = factor
    g.beginPath()
    g.fillStyle = color
    g.rect(xGrid * fact, yGrid * fact, fact, fact)
    g.fill()
    g.closePath()
  def drawDeletePoint(xGrid: Int, yGrid: Int): OnBaseDrawFactor[Unit] =
    gc.clearRect(xGrid * factor, yGrid * factor, factor, factor)

  def updateGridAndDraw(xx : Int,yy : Int) :OnContextUnit= 
    context.grid(xx,yy) = context.color.toString
    drawPoint(xx,yy,context.color.toString)
  

  def resetDataAndDraw(dataS: List[GridValueExport[String]],xSize : Int, ySize : Int): OnContextUnit =
    resetData(dataS,xSize,ySize)
   
  def resierCanvasAndDraw(xSize : Int, ySize : Int):OnBaseDrawFactor[Unit] = 
    canvasOut.width = (xSize * factor.toFloat).round
    canvasOut.height = (ySize * factor.toFloat).round
    draw()
  def resetData(dataS: List[GridValueExport[String]],xSize : Int, ySize : Int): OnBaseDrawFactor[Unit] =
    tContext.grid = MasterGrid(xSize,ySize)
    grid.resetData(dataS)
    
  def draw():OnBaseDrawFactor[Unit] =
    gc.clearRect(0, 0, canvasOut.width, canvasOut.height)
    grid.gridValues().foreach { case Positioned(x, y, color) =>
      drawPoint(x, y, color)

  }
  def updateGrid(xMaxx : Int,yMax : Int): OnContextUnit =
    val nGrid = MasterGrid[String](xMaxx,yMax)
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
    redrawSelsAndSheets()
  extension (s : SheetV)
    def redraw():OnContextUnit = 
      val sheetDiv = s.view
      val p = s.model
      sheetDiv.style.width = s"${ p.width * context.factor}px" 
      sheetDiv.style.height = s"${p.height* context.factor}px" 
      sheetDiv.style.top = s"${p.y* context.factor}px" 
      sheetDiv.style.left = s"${p.x* context.factor}px" 
  def redrawSelsAndSheets():OnContextUnit = 
    context.sheetsMv.foreach(_.redraw())
    context.selections.redraw()
   
  def addSheetUI(p : Positioned[Grid[String]] with Access with AccessVar with Moving[String]):OnContextUnit
}