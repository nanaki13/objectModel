package bon.jo

import bon.jo.words.MathExp
import org.scalajs.dom.HTMLElement
import MiniDsl as !
import !.*
import HtmlPredef.*
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

  def selectAny(v : Any * ):HTMLSelectElement = 

    select_{childs(v.map(v => HtmlPredef.option(me{
      opt => 
        opt.text = v.toString
        opt.value = v.toString
    })).toList)}
  case class GraphViewParam(s : PhraseElement):
    val inputMinX :HTMLInputElement = input(me(_.value = "-10"))
    val inputMaxX :HTMLInputElement = input(me(_.value = "10"))
    val pointNumber :HTMLInputElement = input(me(_.value = "10000"))
    val axePostionX :HTMLSelectElement = selectAny(Dir.middle,Dir.top,Dir.bottom)


    val axePostionY :HTMLSelectElement = selectAny(Dir.middle,Dir.left,Dir.right) 
    def label(l : String)  : HTMLElement= span(_text(l))
    List(inputMinX,inputMaxX,pointNumber).foreach(_.className = "small-input")
    def params:GraphParam = new GraphParam(s,inputMinX.value.toDouble,inputMaxX.value.toDouble,pointNumber.value.toInt,Dir.valueOf(axePostionX.value),Dir.valueOf(axePostionY.value))
    def addTo(n : Element) = n.append(label("min="),inputMinX,label("max="),inputMaxX,label("points="),pointNumber,label("x axis position"),axePostionX,label("y axis position"),axePostionY)
  case class ViewFormule(formule : MathExp.FunctionMathExp):
    val viewModelParam:List[ViewFormuleParam] = formule.symbols.map{
      s => 
          ViewFormuleParam(s , span(_text(s.value+"=")) , input(_class("small-input"),me(_.value = "0")))
    }.toList
    val evaluateButon = button(_text("eval"))
    val resEval = div
    val paramBag = div(childs( 
      div(childs(viewModelParam.map(_.createBag)))
      , div(childs(evaluateButon))
      ,resEval
      ),_class("d-flex"))
    
    val graphButton = button(_text("graph"))


    evaluateButon.addEventListener[MouseEvent]("click", f  => 
      val m = readMap()
      val res = div(childs(
      span(_text( s"f(${formule.symbols.map(m).mkString(", ")})")),
      span(_text("=")), 
      span(_text( evaluate(m).value.toString))))
      resEval.append(res)
    )
 
    val rootUserInput = div(_class("graph-user-input"),childs(
      paramBag
     
    ))
    val root = div(childs(rootUserInput))
    if(formule.symbols.size == 1) then
      
      val info: HTMLElement = div
     
      val bag = div
      bag.style.position = "relative"
      val width = 1000d
      val height = width*9d/16d
      bag.style.width =width+"px"
      bag.style.height=height+"px"
      bag.style.maxWidth =width+"px"
      bag.style.maxHeight=height+"px"
      bag.style.margin = "2em"
      val canvasp = canvas
      
      var ctx = canvasp.getContext("2d").asInstanceOf[CanvasRenderingContext2D];
      canvasp.width = (width).toInt
      canvasp.height = (height).toInt
      val paramsView = GraphViewParam(formule.symbols.head)
      val divUserParams = div(_class("graph-params-user-input"))
      divUserParams.append(graphButton)
      paramsView.addTo(divUserParams)
      rootUserInput.append(divUserParams)
      graphButton.addEventListener[MouseEvent]("click", f  => 
        bag.innerHTML = ""
        bag.append(canvasp)
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
      
    def readMap() : Map[PhraseElement,Double] = viewModelParam.map((p:ViewFormuleParam) => p.param -> p.inputp.value.toDouble).toMap
    def evaluate( m : Map[PhraseElement,Double]) = 
      formule.evaluate(m.mapValues(v => new MathExp.Number(v)).toMap)
  opaque type ViewFormuleParam = (PhraseElement, HTMLElement, HTMLInputElement) 
  object ViewFormuleParam:
    def apply(p :( PhraseElement, HTMLElement, HTMLInputElement) ):ViewFormuleParam = p
    extension (p : ViewFormuleParam)
      inline def param = p._1
      inline def label : HTMLElement = p._2
      inline def inputp : HTMLInputElement = p._3
      inline def createBag : HTMLElement = div(childs(p.label,div(childs(p.inputp),_class("t-right"))),_class("d-flex j-c-end"))
  def apply(formule : MathExp.FunctionMathExp):ViewFormule=  ViewFormule(formule)
