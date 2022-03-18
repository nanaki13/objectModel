package bon.jo

import bon.jo.words.MathExp
import org.scalajs.dom.HTMLElement
import MiniDsl as !
import !.{me,_text,childs}
import org.scalajs.dom.HTMLInputElement
import bon.jo.words.PhraseElement
import View.ViewFormuleParam.*
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.console.log
import org.scalajs.dom.MouseEvent
import bon.jo.Graph.GraphParam
import bon.jo.Graph.scaleToMe
import org.scalajs.dom.Node
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.HTMLSelectElement
import org.scalajs.dom.HTMLOptionElement
import bon.jo.HtmlViewGraph.DrawerParam
import bon.jo.HtmlViewGraph.Drawer
import org.scalajs.dom.HTMLDivElement
import  HtmlViewGraph.{drawGraphValues,drawYTick,drawXTick}
import bon.jo.Graph.Dir
object View :

  def select(v : Any * ):HTMLSelectElement = !.select[HTMLSelectElement](childs(v.map(v => !.option[HTMLOptionElement](me{
      opt => 
        opt.text = v.toString
        opt.value = v.toString
    })).toList))
  case class GraphViewParam(s : PhraseElement):
    val inputMinX :HTMLInputElement = !.input(me(_.value = "-10"))
    val inputMaxX :HTMLInputElement = !.input(me(_.value = "10"))
    val pointNumber :HTMLInputElement = !.input(me(_.value = "10000"))
    val axePostionX :HTMLSelectElement = select(Dir.middle,Dir.top,Dir.bottom)


    val axePostionY :HTMLSelectElement = select(Dir.middle,Dir.left,Dir.right) 
    def label(l : String)  : HTMLElement= !.span[HTMLElement](_text(l))
    List(inputMinX,inputMaxX,pointNumber).foreach(_.className = "small-input")
    def params:GraphParam = new GraphParam(s,inputMinX.value.toDouble,inputMaxX.value.toDouble,pointNumber.value.toInt,Dir.valueOf(axePostionX.value),Dir.valueOf(axePostionY.value))
    def addTo(n : Element) = n.append(label("min="),inputMinX,label("max="),inputMaxX,label("points="),pointNumber,label("x axis position"),axePostionX,label("y axis position"),axePostionY)
  case class ViewFormule(formule : MathExp.FunctionMathExp):
    val viewModelParam:List[ViewFormuleParam] = formule.symbols.map{
      s => 
          ViewFormuleParam(s , !.span[HTMLElement](_text(s.value+"=")) , !.input[HTMLInputElement])
    }.toList
    val paramBag = !.div[HTMLElement](childs(viewModelParam.map(_.createBag).toList))
    val evaluateButon = !.button[HTMLButtonElement](_text("eval"))
    val graphButton = !.button[HTMLButtonElement](_text("graph"))


    evaluateButon.addEventListener[MouseEvent]("click", f  => 
      val m = readMap()
      val res = !.div[HTMLElement](childs(
      !.span[HTMLElement](_text( s"f(${formule.symbols.map(m).mkString(", ")})")),
      !.span[HTMLElement](_text("=")), 
      !.span[HTMLElement](_text( evaluate(m).value.toString))))
      paramBag.append(res)
    )
 
    val root : HTMLElement = !.div[HTMLElement](childs(paramBag,evaluateButon))
    if(formule.symbols.size == 1) then
      
      val info: HTMLElement = !.div[HTMLElement]
      root.append(graphButton)
      val bag = !.div[HTMLDivElement]
      bag.style.position = "relative"
      val width = 1000d
      val height = width*9d/16d
      bag.style.width =width+"px"
      bag.style.height=height+"px"
      bag.style.maxWidth =width+"px"
      bag.style.maxHeight=height+"px"
      bag.style.margin = "2em"
      val canvas = !.canvas[HTMLCanvasElement]
      
      var ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D];
      canvas.width = (width).toInt
      canvas.height = (height).toInt
      val paramsView = GraphViewParam(formule.symbols.head)
      paramsView.addTo(root)
      graphButton.addEventListener[MouseEvent]("click", f  => 
        bag.innerHTML = ""
        bag.append(canvas)
        ctx.clearRect(0,0,width,height)
        val params = paramsView.params
        println(params)
        val values = Graph.values(formule,params)
   
        val maxY = values.max((a,b) => a.funVal.compareTo(b .funVal)).funVal
        val minY = values.min((a,b) => a.funVal.compareTo(b .funVal)).funVal
        given Drawer[HTMLDivElement] = HtmlViewGraph.Drawer(DrawerParam(params,minY,maxY,width,height),info)
        given Drawer[CanvasRenderingContext2D] = HtmlViewGraph.Drawer.canvas(DrawerParam(params,minY,maxY,width,height))
        

        val fx = width.scaleToMe(params.min,params.max,-params.min)
        val scaleYCtx : Double => Double =  v => height.scaleToMe(minY,maxY,maxY)(-v)
        val scaleXCtx : Double => Double =  fx
        val scaleY : Double => Double =  height.scaleToMe(minY,maxY,-minY)
        val scaleX : Double => Double = fx
        val xTick = (params.max - params.min)/10.0
        val yTick = (maxY -minY ) /10.0
        bag.drawXTick()
        bag.drawYTick()
        bag.drawGraphValues(values)
        ctx.drawXTick()
        ctx.drawYTick()
        ctx.drawGraphValues(values)
      )
      root.append(bag)
      root.append(info)
      
    def readMap() : Map[PhraseElement,Double] = viewModelParam.map((p:ViewFormuleParam) => p.param -> p.input.value.toDouble).toMap
    def evaluate( m : Map[PhraseElement,Double]) = 
      formule.evaluate(m.mapValues(v => new MathExp.Number(v)).toMap)
  opaque type ViewFormuleParam = (PhraseElement, HTMLElement, HTMLInputElement) 
  object ViewFormuleParam:
    def apply(p :( PhraseElement, HTMLElement, HTMLInputElement) ):ViewFormuleParam = p
    extension (p : ViewFormuleParam)
      inline def param = p._1
      inline def label : HTMLElement = p._2
      inline def input : HTMLInputElement = p._3
      inline def createBag : HTMLElement = !.div[HTMLElement](childs(p.label,p.input))
  def apply(formule : MathExp.FunctionMathExp):ViewFormule=  ViewFormule(formule)
