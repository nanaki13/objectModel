package bon.jo
import MiniDsl as !
import HtmlPredef.*
import !.*
import org.scalajs.dom.HTMLElement
import bon.jo.Graph.scaleToMe
import bon.jo.Graph.GraphParam
import bon.jo.Graph.EvaluedFun1
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.HTMLDivElement
import bon.jo.Graph.Dir
object HtmlViewGraph :

  extension [T](t : T)(using d : Drawer[T])
    inline def drawXTick():Unit = d.drawXTick(t)
    inline def drawYTick():Unit = d.drawYTick(t) 
    inline def drawGraphValues(values : List[EvaluedFun1]) :Unit = d.drawGraphValues(values)(t)
  case class DrawerParam( params: GraphParam,
                  minY:Double,
                  maxY:Double  ,
                  width : Double,
                  height : Double)
  object Drawer:
    def canvas( paramsp: DrawerParam):Drawer[CanvasRenderingContext2D]=
      new CanvasDrawer:       
          lazy val params = paramsp
    def apply(paramsp: DrawerParam,infop :HTMLElement):Drawer[HTMLDivElement]=
      new DivDrawer:
        val info = infop
        lazy val params = paramsp
        
      
  trait Drawer[-T]:
    def drawXTick(t : T):Unit
    def drawYTick(t : T):Unit 
    def drawGraphValues(values : List[EvaluedFun1])(t : T) :Unit
    lazy val params: DrawerParam
    lazy val xTick = (params.params.max - params.params.min)/10.0
    lazy val yTick = (params.maxY -params.minY ) /10.0
    def scaleX : Double => Double = params.width.scaleToMe(params.params.min,params.params.max,-params.params.min)
    def scaleY : Double => Double
    def overXTick(f : Double => Unit) = Iterator.iterate(params.params.min)(_ + xTick).takeWhile(_ <= params.params.max).foreach(f)
    def overYTick(f : Double => Unit) = Iterator.iterate(params.minY)(_ + yTick).takeWhile(_ <= params.maxY).foreach(f)
    def xTickY():Double = params.params.axePostionX match
      case Dir.top => params.height
      case Dir.middle => params.height/2
      case Dir.bottom => 0
      case _ => throw new IllegalStateException("xTickY cant be "+params.params.axePostionX)
    def yTickX():Double  = params.params.axePostionY match
      case Dir.right => params.width
      case Dir.middle => params.width/2
      case Dir.left => 0
      case _ => throw new IllegalStateException("yTickX cant be "+params.params.axePostionY)
  trait DivDrawer extends Drawer[HTMLDivElement]:
    val info :HTMLElement
    def scaleY : Double => Double = params.height.scaleToMe(params.minY,params.maxY,-params.minY)
    def drawXTick( t : HTMLDivElement):Unit = 
      val yPos = xTickY()
      overXTick{
         e =>   
          val pt = div
          pt.style.position = "absolute"
          pt.textContent = f"${e}%.2f".toString
          pt.style.fontSize = "0.5em"
          pt.style.bottom =  yPos+"px"
          pt.style.left= scaleX(e)+"px"
        //  t.append(pt) 
      }

    def drawYTick(t : HTMLDivElement):Unit = 
      val xPos = yTickX()
      overYTick{
        e =>       
          val pt = div
          pt.style.position = "absolute"
          pt.textContent = f"${e}%.2f".toString
          pt.style.fontSize = "0.5em"
          pt.style.left =  xPos+"px"
          pt.style.bottom= scaleY(e)+"px"
       //   t.append(pt)
      }  
    def drawGraphValues(values : List[EvaluedFun1])(t : HTMLDivElement) :Unit = 
      values.foreach{
          e => 
            val pt = div(_class("point"))       
            pt.onmouseenter = f => {
              info.textContent = s"f(${e.paramVal})=${e.funVal}"     
            
            }
           
            pt.style.bottom = scaleY( e.funVal)+"px"
            pt.style.left= scaleX(e.paramVal)+"px"
            t.append(pt)
      }  
  trait CanvasDrawer extends Drawer[CanvasRenderingContext2D]:
    def scaleY : Double => Double =  v => 
      val ret = params.height.scaleToMe(params.minY,params.maxY,params.maxY)(-v)
      if ret > 0 then ret else 1

    def drawXTick( t : CanvasRenderingContext2D):Unit = 
      val yPos = params.height - xTickY()
      t.beginPath()
      t.moveTo(0,yPos)
      t.lineTo(params.width,yPos)
      t.stroke()
      t.closePath()
      overXTick{
        e =>   
          t.beginPath()
          val x = scaleX(e)
          t.moveTo(x,yPos-10)
          t.lineTo(x,yPos+10)
          t.stroke()
          t.closePath()
          t.fillText(f"${e}%.2f".toString,x,yPos)

      }

    def drawYTick(t : CanvasRenderingContext2D):Unit = 
      val xPos = yTickX()
      t.beginPath()
      t.moveTo(xPos,0)
      t.lineTo(xPos,params.height)
      t.stroke()
      t.closePath()
      overYTick{
        e =>  
          val y = scaleY(e)
          t.moveTo(xPos-10,y)
          t.lineTo(xPos+10,y)
          t.stroke()
          t.closePath()     
          t.fillText(f"${e}%.2f".toString,xPos, y)
          println((e,scaleY(e)))
      }  

  
    def drawGraphValues(values : List[EvaluedFun1])(t : CanvasRenderingContext2D) :Unit = 
      t.beginPath()
      values.foreach{
        e => 
        
          t.lineTo(scaleX(e.paramVal),scaleY( e.funVal))

      }
      t.stroke()
      t.closePath()
      
    