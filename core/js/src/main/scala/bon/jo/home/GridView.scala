package bon.jo.home

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.console
import bon.jo.Draw.Grid
import org.scalajs.dom.HTMLCanvasElement
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import org.scalajs.dom.PointerEvent
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.CanvasRenderingContext2D

object GridView:
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
              posConsumer(nH)
              varval.value = nH
              
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
    var r = 0d
    val gPicker = input(me(_.style.width = "30px"))
    val bPicker = input(me(_.style.width = "30px"))
    val colPi = div(childs(colorPicker,div(childs(rPicker,gPicker,bPicker))))
    var colot: Color = Color.RGB(0,0,0)
    def hex(c : Color.RGB) =  

      String.format("#%02x%02x%02x", c.r,  c.g, c.b)
   
    def updateColor(c : Color):Unit = 
      colot = c
      colorPicker.style.backgroundColor = colot.toString
    def colorChage() = 
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
    val fact = 10
    val grid = Grid[String](40,40)
    var mousedown = false

    def draw(ev : MouseEvent) =
        given MouseEvent = ev
        given HTMLElement = c
        context.beginPath()
        context.fillStyle=colot.toString
        context.rect(x,y,fact,fact)
        context.fill()
        context.closePath()

    def x(using ev : MouseEvent,c : HTMLElement ) = (((ev.pageX - c.offsetLeft)/fact)).toInt * fact
    def y(using ev : MouseEvent,c : HTMLElement ) = (((ev.pageY - c.offsetTop)/fact)).toInt * fact
    lazy val context =  c.getContext("2d").asInstanceOf[ CanvasRenderingContext2D]
    lazy val c : HTMLCanvasElement = canvas(click{draw},me(_.width=40*fact),me(_.height=40*fact))
    c.onmousemove = e => if mousedown then draw(e)
    c.onmousedown = _ => mousedown = true
    c.onmouseup = _ => mousedown = false
    c.style.backgroundColor = "red"
    val ret = div(childs(c,colPi))
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
    ret.append(cRed, cGreen, cBlues,cH,cS,cL )
    ret
    
    
