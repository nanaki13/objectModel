package bon.jo.home

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.console
import bon.jo.Draw.Grid
import bon.jo.Draw.toAllObj
import org.scalajs.dom.HTMLCanvasElement
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import org.scalajs.dom.PointerEvent
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.CanvasRenderingContext2D
import bon.jo.Draw.GridValue
 import bon.jo.Draw.toGridValueExport
import bon.jo.objects.All.toJsonString
import java.util.Base64
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.FileReader
import bon.jo.objects.All
import bon.jo.Draw.GridValueExport
import bon.jo.Draw.Positioned
import bon.jo.home.GridView.Context
import bon.jo.home.GridView.Context.*
import bon.jo.HtmlEvent.EventAdder
object GridView:

  
  class Context(val grid : Grid[String],val c : HTMLCanvasElement,var factor : Double,var color : Color):
    val gc =  c.getContext("2d").asInstanceOf[ CanvasRenderingContext2D]
  object Context:
    type OnContext[A] = Context ?=> A
    type OnContextUnit = OnContext[Unit]
    inline def apply():OnContext[Context] = summon
    
  def x(using ev : MouseEvent,c : HTMLElement ) = ev.pageX - c.offsetLeft
  def y(using ev : MouseEvent,c : HTMLElement ) =ev.pageY - c.offsetTop
  inline def context:OnContext[Context] = Context()
  def nothing(ev : MouseEvent):OnContextUnit={}
  def draw(ev : MouseEvent):OnContextUnit=
    given MouseEvent = ev
    given HTMLElement =context.c
    val fact = context.factor
    val grid = context.grid
    val colot = context.color
    val x_ = (x/fact).toInt
    val y_ = (y/fact).toInt
    grid(x_,y_) = GridValue(colot.toString)
    drawPoint(x_,y_,colot.toString)
      
  def prepare(c : HTMLCanvasElement,cWidth : Int , cHeight : Int): OnContextUnit = 
    given HTMLCanvasElement = c
    EventAdder.click(draw)
    c.width=cWidth
    c.height=cHeight
    c.style.backgroundColor="white"
  def drawPoint(xGrid: Int,yGrid: Int,color : String):OnContextUnit =
    val g = context.gc
    val fact = context.factor
    g.beginPath()
    g.fillStyle=color
    g.rect( xGrid * fact, yGrid * fact,fact,fact)
    g.fill()
    g.closePath()
  enum Color:
    case RGB(r:Int,g:Int,b:Int) 
    case HSL(h:Double,s:Double,l:Double) 
    override def toString():String = 
      this match
        case RGB(r,g,b) => s"rgb($r,$g,$b)"
        case HSL(h,s,l) => s"hsl(${h} ${s}% ${l}%)"

  def view():HTMLElement = 
    inline val cntColorStep = 500
    inline val colorNum = 255 * 255 *255
    inline val colrStep =  360d / 500d
    def arc: Seq[(Double)] = (for{
      c <- 0 until 500
    
    } yield c) map(_ * colrStep)

    def colorLine() = div(childs( arc.map {
      (c) => 
       
        val s = s"hsl(${c} 100% 50%)"
        
        s
    }.map{
      cssClor => 
        val spanc =  span
        spanc.style.backgroundColor = cssClor
        spanc.style.width = "1px"
        spanc.style.height = "15px"
        spanc.style.display =  "inline-block"
        spanc
    }))

    def colorLineCanvas(posConsumer : Double => Unit):(VarValueDouble,HTMLCanvasElement) = 
      val varval = VarValue(0d)
      val canvasp = canvas(me(
        can =>
          can.width = cntColorStep
          can.height = 15
          import bon.jo.HtmlEvent.onmousedownandmove
          def x(using ev : MouseEvent,c : HTMLElement ) = ev.pageX - c.offsetLeft
          def y(using ev : MouseEvent,c : HTMLElement ) = ev.pageY - c.offsetTop
          can.onmousedownandmove{
            e =>
              given MouseEvent = e
              given HTMLElement = can
              val nH = x*colrStep
              varval.value = nH
              posConsumer(nH)
                  
          }
          val context =  can.getContext("2d").asInstanceOf[ CanvasRenderingContext2D]
        
          arc.map {
            (c) => 
            
              val s = s"hsl(${c} 100% 50%)"
            
              s
          }.zipWithIndex.foreach{
            (cssClor, i) => 
              context.beginPath()
              context.fillStyle=cssClor
              context.rect(i,0,1,15)
              context.fill()
              context.closePath()
          }
        )
      )
      (varval,canvasp)

    inline def hslColor100_50(h : Double): String  = s"hsl(${h} 100% 50%)"
    inline def hslColor(h : Double, s : Double, l : Double) : String = s"hsl(${h} ${s}% ${l}%)"

    val colorPicker = div(me(_.style.width = "30px"),me(_.style.height = "30px"))
    
    val rPicker = input(me(_.style.width = "30px"))
    val gPicker = input(me(_.style.width = "30px"))
    val bPicker = input(me(_.style.width = "30px"))
    val colPi = div(childs(colorPicker,div(childs(rPicker,gPicker,bPicker))))

    def hex(c : Color.RGB) =  

      String.format("#%02x%02x%02x", c.r,  c.g, c.b)
   

    val fact = 10
    val gridX = 40
    val gridY = 40
    var grid = Grid[String](gridX,40)
    val cWidth = gridX*fact
    val cHeight =  gridY*fact
    var mousedown = false






    
    val myCanvas : HTMLCanvasElement = canvas
    given Context = new Context(grid,myCanvas,fact,Color.RGB(0,0,0))
    def updateColor(c : Color):OnContextUnit = 
      context.color = c
      colorPicker.style.backgroundColor =c.toString
    def colorChage():OnContextUnit = 
      updateColor(Color.RGB(rPicker.value.toInt,gPicker.value.toInt,bPicker.value.toInt))
      
    rPicker.onchange = {
      _ =>
       colorChage()
    }
    gPicker.onchange = {
      _ =>
       colorChage()
    }
    bPicker.onchange = {
      _ =>
       colorChage()
    }
    prepare(myCanvas,cWidth,cHeight)

    var currentProcess : (MouseEvent) => OnContextUnit  = draw
    myCanvas.onmousemove = e => if mousedown then currentProcess(e)
    myCanvas.onmousedown = _ => mousedown = true
    myCanvas.onmouseup = _ => mousedown = false
    
    given (String => HTMLElement) = i => div(_text(i.toString))
    given PaletteContext = PaletteContext("tool-select","row","cell")
    val palette = Palette(2,"Draw","Select")

    
    palette.listen = {
      case "Draw" =>  currentProcess = draw
      case "Select" =>   currentProcess = nothing
    }
    val ret = div(childs( myCanvas, palette.root,colPi))

    def save():OnHTMLElement = 
      val s = grid.json().toJsonString()  
      val aLink = a( _text("Download"), me(_.asInstanceOf[scalajs.js.Dynamic].download = "image.json" ), me(_.href=s"""data:application/json;base64,${Base64.getEncoder.encodeToString(s.getBytes)}"""))
      OnHtml().append(div(childs(aLink)))


    val buttonSave = button(_text("save"),click(save()(using ret)))

    val i = input(me(_.`type`="file"),me(_.name="img-json"),me(_.id="img-json"),me(_.style.display="none"))
    i.onchange = e => {
      val f : FileReader = new FileReader()
      f.onloadend = l => 
        val dataJson : All.ListAll[String] = All(f.result.toString).asInstanceOf
        val dataS : List[GridValueExport[String]] = dataJson.value.map(_.asInstanceOf[All.ObjectAll[String]].toGridValueExport)
        resetData(dataS)

      f.readAsText(i.files(0),"utf-8")
    }
    inline def open():HTMLDivElement = div(childs(button(childs(label(_text("Open"),me(_.htmlFor="img-json")),i) ))) 

    def resetData( dataS : List[GridValueExport[String]]):OnContextUnit =
      context.gc.clearRect(0,0,cWidth,cHeight)
      grid.resetData(dataS)
      grid.gridValues().foreach{
        case Positioned(x,y,color) => drawPoint(x,y,color)

      }

    lazy val (vRed : VarValueDouble,cRed) = Cursor(f => 
      updateColor(Color.RGB((f*255).round.toInt,(vGreen.value*255).round.toInt,(vBlue.value*255).round.toInt))
      ,ret)
    lazy val (vGreen : VarValueDouble,cGreen) = Cursor(f => 
      updateColor( Color.RGB( (vRed.value*255).round.toInt,(f*255).round.toInt,(vBlue.value*255).round.toInt))
    ,ret)
    lazy val (vBlue : VarValueDouble,cBlues) = Cursor(f => 
      updateColor(Color.RGB( (vRed.value*255).round.toInt,(vGreen.value*255).round.toInt,(f*255).round.toInt))
      ,ret)
    
    lazy val (vS : VarValueDouble,cS) = Cursor(s => 
      updateColor( Color.HSL(vH.value,s*100,vL.value*100))
      ,ret)
    lazy val (vL : VarValueDouble,cL) = Cursor(l => 
      updateColor( Color.HSL(vH.value,vS.value*100,l*100))
      ,ret)
    lazy val (vH : VarValueDouble,cH) =colorLineCanvas(h => 
       updateColor( Color.HSL(h,vS.value*100,vL.value*100))
    )
    ret.append(cRed, cGreen, cBlues,cH,cS,cL,div(childs(buttonSave)),open() )
    ret
    
    
