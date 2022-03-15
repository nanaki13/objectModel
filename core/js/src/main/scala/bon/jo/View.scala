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
import org.scalajs.dom.Node
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.HTMLSelectElement
import org.scalajs.dom.HTMLOptionElement
object View :
  def select(v : String * ):HTMLSelectElement = !.select[HTMLSelectElement](childs(v.map(v => !.option[HTMLOptionElement](me{
      opt => 
        opt.text = v
        opt.value = v
    })).toList))
  case class GraphViewParam(s : PhraseElement):
    val inputMinX :HTMLInputElement = !.input(me(_.value = "-10"))
    val inputMaxX :HTMLInputElement = !.input(me(_.value = "10"))
    val pointNumber :HTMLInputElement = !.input(me(_.value = "10000"))
    val axePostionX :HTMLSelectElement = select("middle","left","right")


    val axePostionY :HTMLSelectElement = select("middle","top","bottom")
    def label(l : String)  : HTMLElement= !.span[HTMLElement](_text(l))
    List(inputMinX,inputMaxX,pointNumber).foreach(_.className = "small-input")
    def params:GraphParam = new GraphParam(s,inputMinX.value.toDouble,inputMaxX.value.toDouble,pointNumber.value.toInt)
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
      val bag = !.div[HTMLElement]
      bag.style.position = "relative"
      val width = 500d
      val height = 300d
      bag.style.width =width+"px"
      bag.style.height=height+"px"
      bag.style.margin = "2em"
      val canvas = !.canvas[HTMLCanvasElement]
     // canvas.style.width =(width*2)+"px"
      // canvas.style.height=(width*2)+"px"
      
      var ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D];
      canvas.width = width.toInt
      canvas.height = height.toInt
      val paramsView = GraphViewParam(formule.symbols.head)
      paramsView.addTo(root)
      graphButton.addEventListener[MouseEvent]("click", f  => 
        bag.innerHTML = ""
        bag.append(canvas)
        
        val params = paramsView.params
        println(params)
        val values = Graph.values(formule,params)
   
        val maxY = values.max((a,b) => a.funVal.compareTo(b .funVal)).funVal
        val minY = values.min((a,b) => a.funVal.compareTo(b .funVal)).funVal
        println((minY,maxY))
        val scaleYCtx : Double => Double =  v => (-v +maxY) *  height/(maxY -minY ) 
        val scaleXCtx : Double => Double =  v => (v - params.min) *  width/(params.max -params.min ) 
        val scaleY : Double => Double =  v => (v-minY) *  height/(maxY -minY ) 
        val scaleX : Double => Double =  v => (v - params.min) *  width/(params.max -params.min ) 
        val xTick = (params.max - params.min)/10.0
        val yTick = (maxY -minY ) /10.0
        ctx.beginPath();
        ctx.ellipse(0,0,10,10,0,0,Math.PI * 2,false)
        ctx.fill()
        Iterator.iterate(params.min)(_ + xTick).takeWhile(_ <= params.max).foreach{
            e => 
              val pt = !.div[HTMLElement]
              pt.style.position = "absolute"
              pt.textContent = f"${e}%.2f".toString
              pt.style.fontSize = "0.5em"
              pt.style.bottom =  "-1em"
              pt.style.left= scaleX(e)+"px"
              ctx.fillText(f"${e}%.2f".toString,scaleXCtx(e), scaleYCtx(0))
              bag.append(pt)
          }
        Iterator.iterate(minY)(_ + yTick).takeWhile(_ <= maxY).foreach{
          e => 
           
            
            val pt = !.div[HTMLElement]
            pt.style.position = "absolute"
            pt.textContent = f"${e}%.2f".toString
            pt.style.fontSize = "0.5em"
            pt.style.left =  "-1em"
            pt.style.bottom= scaleY(e)+"px"
            ctx.fillText(f"${e}%.2f".toString,scaleXCtx(0), scaleYCtx(e))
            bag.append(pt)
        }
       // ctx.scale()
        ctx.beginPath()
      //  ctx.strokeStyle = "black"
        values.foreach{
          e => 
            val pt = !.div[HTMLElement]
            ctx.lineTo(scaleXCtx(e.paramVal),scaleYCtx( e.funVal))
            pt.style.position = "absolute"
            pt.style.height = "2px"
            pt.style.width = "2px"
            pt.onmouseenter = f => {
              info.textContent = s"f(${e.paramVal})=${e.funVal}"     
              pt.style.height = "4px"
              pt.style.width = "4px" 
              pt.style.backgroundColor = "red"
            }
            pt.onmouseleave = f => {
              info.textContent = s"f(${e.paramVal})=${e.funVal}"     
              pt.style.height = "2px"
              pt.style.width = "2px" 
              pt.style.backgroundColor = "black"
            }
            pt.style.backgroundColor = "black"
            pt.style.bottom = scaleY( e.funVal)+"px"
            pt.style.left= scaleX(e.paramVal)+"px"
            bag.append(pt)
        }
        ctx.stroke()
        ctx.closePath()
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
