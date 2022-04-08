package bon.jo.home

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.console
import scalajs.js.JSON
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
import bon.jo.Draw.EmptyGridElement
import org.scalajs.dom.TouchList
import org.scalajs.dom.TouchEvent
object GridView:

  sealed trait Sel
  case class RectSelect(xIni: Int, yIni: Int, xEnd: Int, yEnd: Int) extends Sel
  trait ActionParam
  case class SelectActionParam(
      var begin: Boolean,
      selectDiv: HTMLElement,
      var xIni: Int,
      var yIni: Int
  ) extends ActionParam
  object NoActionParam extends ActionParam
  trait ProcessEvent:
    def process(xI : Int,yI:Int): OnContextUnit
    def start(xI : Int,yI:Int): OnContextUnit
    def end(xI : Int,yI:Int): OnContextUnit
  object DrawProcessEvent extends ProcessEvent:
    def process(xI : Int,yI:Int): OnContextUnit = draw(xI, yI)
    def start(xI : Int,yI:Int): OnContextUnit = ()
    def end(xI : Int,yI:Int): OnContextUnit = ()
  object PasteProcessEvent extends ProcessEvent:
    def process(xI : Int,yI:Int): OnContextUnit =      
      paste(xI,yI)
    def start(xI : Int,yI:Int): OnContextUnit = ()
    def end(xI : Int,yI:Int): OnContextUnit = ()
  object SelectProcessEvent extends ProcessEvent:
    def process(xI : Int,yI:Int): OnContextUnit = 
      
      selectAction(xI,yI)
    def start(xI : Int,yI:Int): OnContextUnit =
      context.actionParam =
        SelectActionParam(true, div(_class("select-rect")), 0, 0)
    def end(xI : Int,yI:Int): OnContextUnit =
     
      given HTMLElement = context.parentCanvas
      val param = context.actionParam.asInstanceOf[SelectActionParam]
      context.selections.add(
        RectSelect(param.xIni, param.yIni, xI, yI),
        param.selectDiv
      )
  class Context(
      val grid: Grid[String],
      val canvas: HTMLCanvasElement,
      val colorPicker: HTMLElement,
      var factor: Double,
      var color: Color,
      var currentProcess: ProcessEvent,
      var actionParam: ActionParam,
      var savedColor : List[Color],
      var gridsCopy :List[Positioned[Grid[String]]] = Nil
  ):
    val gc = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    def parentCanvas = canvas.parentElement
    object selections:
      var select: List[(Sel, HTMLElement)] = Nil
      def add(sel: Sel, el: HTMLElement) =
        select = select :+ sel -> el
      def clear() =
        select.foreach { (_, e) =>
          parentCanvas.removeChild(e)
        }
        select = Nil

  object Context:
    type OnContext[A] = Context ?=> A
    type OnContextUnit = OnContext[Unit]
    inline def apply(): OnContext[Context] = summon
  def xyInGrid( ev: MouseEvent): OnContext[(Int,Int)] = ((xme(ev) / context.factor).toInt, (yme(ev) / context.factor).toInt)
  def xyInGrid( ev: TouchEvent): OnContext[(Int,Int)] = 
    val (xx,yy) = xy(ev)
    ((xx/context.factor.toInt),(yy/context.factor).toInt)

  def xInGrid(x: Int): OnContext[Int] = (x / context.factor).toInt
  def yInGrid(y: Int): OnContext[Int] = (y / context.factor).toInt
  def xme( ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetX.asInstanceOf[Int]
  def yme( ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetY.asInstanceOf[Int]
  def xy( ev: TouchEvent): (Int,Int) =
    val rect = ev.target.asInstanceOf[HTMLElement].getBoundingClientRect()
    val t  = if !scalajs.js.isUndefined( ev.targetTouches(0)) then  ev.targetTouches(0) else ev.changedTouches(0)
    console.log(ev)
    ((t.pageX - rect.left).toFloat.round,(t.pageY - rect.top).toFloat.round)

  inline def context: OnContext[Context] = Context()
  def selectAction(xI : Int,yI : Int): OnContextUnit = {

    val param: SelectActionParam = context.actionParam.asInstanceOf
    //given MouseEvent = ev
    given HTMLElement = context.canvas
    if param.begin then
      param.begin = false
      param.selectDiv.style.left = s"${xI*context.factor}px"
      param.selectDiv.style.top = s"${yI*context.factor}px"
      param.xIni = xI
      param.yIni = yI
      context.parentCanvas.append(param.selectDiv)
    else
      var wR = xI - param.xIni
      var hR = yI - param.yIni
      hR = if hR < 0 then
        param.selectDiv.style.top = s"${(param.yIni + hR)*context.factor}px"
        -hR
      else
        param.selectDiv.style.top = s"${param.yIni*context.factor}px"
        hR
      wR = if wR < 0 then
        param.selectDiv.style.left = s"${(param.xIni + wR)*context.factor}px"
        -wR
      else
        param.selectDiv.style.left = s"${param.xIni*context.factor}px"
        wR

      param.selectDiv.style.width = s"${wR*context.factor}px"
      param.selectDiv.style.height = s"${hR*context.factor}px"

  }
  def copySel():OnContextUnit = 
    context.gridsCopy = Nil
    doOnSelctedcoords((xx,yy)=> 
      println((xx,yy))
      console.log(context.grid.data)
      context.grid(xx,yy) match
        case value :  GridValue[_] => 
            val g = context.gridsCopy.head
            console.log(g.v.data)
            g.v(xx-g.x,yy-g.y) = value
        case o =>
      ,(xMin : Int,xMax : Int,yMin : Int,yMax : Int) =>  {
        context.gridsCopy = Positioned(xMin,yMin,Grid(xMax - xMin,yMax - yMin)) :: context.gridsCopy
      }   )
  def paste(x : Int,y : Int):OnContextUnit = 
      context.gridsCopy.foreach{
        pos => pos.v.gridValues().foreach{
          gvPos => 
            val xx = x+gvPos.x
            val yy = x+gvPos.x
            if xx < context.grid.xMax && yy < context.grid.yMax then
              context.grid(x+gvPos.x,y+gvPos.y) = gvPos.v
        }
      }
      draw()
  def draw(xI : Int,yI : Int): OnContextUnit =
    given HTMLElement = context.canvas
    val fact = context.factor
    val grid = context.grid
    val colot = context.color
   
    val y_ = yInGrid
    grid(xI, yI) = GridValue(colot.toString)
    drawPoint(xI, yI, colot.toString)

  def prepare(c: HTMLCanvasElement, cWidth: Int, cHeight: Int): OnContextUnit =
    given HTMLCanvasElement = c

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
  enum Color:
    case RGB(r: Int, g: Int, b: Int)
    case HSL(h: Double, s: Double, l: Double)
    case Raw(cssColorDef : String)
    override def toString(): String =
      this match
        case RGB(r, g, b) => s"rgb($r,$g,$b)"
        case HSL(h, s, l) => s"hsl(${h} ${s}% ${l}%)"
        case Raw(c) => c

  def saveColor(out : HTMLElement): OnContextUnit = 
    val saved = context.color
    saveColor(saved,out)
  inline def tillColor() =  div(_class("color-till"))
  def  colorFromDraw(outPallette : HTMLElement):OnContextUnit = 
    val savedSet = context.savedColor.toSet
    println(savedSet)
    context.grid.data.filter(_ != EmptyGridElement).map(_.asGridValue[String]().v).toSet.map{
      v => 
        val c = Color.Raw(v)
        println(c)
        c
    }.filter(!savedSet.contains(_)).foreach(saveColor(_,outPallette))
  def updateColor(c: Color): OnContextUnit =
    context.color = c
    context.colorPicker.style.backgroundColor = c.toString
  def saveColor(saved : Color, out : HTMLElement): OnContextUnit = 
    context.savedColor = context.savedColor :+ saved
    val d = tillColor()
    out.append(d)
    d.style.backgroundColor = saved.toString
    EventAdder.click(_ => 
      updateColor(saved)
    )(using d)
  def updateGridAndDraw(xx : Int,yy : Int) :OnContextUnit= 
    context.grid(xx,yy) = context.color.toString
    drawPoint(xx,yy,context.color.toString)
  def applyCuurentColorToSel():OnContextUnit = 
    doOnSelctedcoords((xx,yy)=> 
      context.grid(xx,yy) match
        case value :  GridValue[_] => 
            updateGridAndDraw(xx,yy)
        case o =>
      ,(_,_,_,_) => ()   )
  def strokeSel():OnContextUnit =
    context.selections.select.foreach {
      case (RectSelect(xi, yi, xe, ye), _) => 
        val xMin = Math.min(xi, xe)
        val yMin = Math.min(yi, ye)
        val xMax = Math.max(xi, xe) 
        val yMax = Math.max(yi, ye) 
        for {
          xx <- xMin until xMax
        } {
          
          updateGridAndDraw(xx,yMin)
          updateGridAndDraw(xx,yMax-1)
        }
        for {
          yy <- yMin until yMax
        } {
          
          updateGridAndDraw(xMin,yy)
          updateGridAndDraw(xMax-1,yy)
        }
      }
  def fillSel():OnContextUnit =
    context.selections.select.foreach {
      case (RectSelect(xi, yi, xe, ye), _) => 
        val xMin = Math.min(xi, xe)
        val yMin = Math.min(yi, ye)
        val xMax = Math.max(xi, xe) 
        val yMax = Math.max(yi, ye) 
        for {
          xx <- xMin until xMax
          yy <- yMin until yMax
        } yield {
          updateGridAndDraw(xx,yy)
        }
      }
  


  def doOnSelctedcoords[A](f : (Int,Int)=> A,sel : (xMin : Int,xMax : Int,yMin : Int,yMax : Int) => Unit):OnContext[List[Seq[A]]]=
    context.selections.select.map {
      case (RectSelect(xi, yi, xe, ye), _) =>
        val xMin = Math.min(xi, xe)
        val yMin = Math.min(yi, ye)
        val xMax = Math.max(xi, xe) 
        val yMax = Math.max(yi, ye) 
        sel(xMin,xMax,yMin,yMax)
        for {
          xx <- xMin until xMax
          yy <- yMin until yMax
        } yield {
          f(xx,yy)
        }
    }
  def deleteSele(): OnContextUnit =
    doOnSelctedcoords(context.grid(_, _) = EmptyGridElement,(xMin : Int,xMax : Int,yMin : Int,yMax : Int) =>  {
      context.gc.clearRect(
          xMin * context.factor,
          yMin * context.factor,
          (xMax - xMin) * context.factor,
          (yMax - yMin) * context.factor
        )   
    })
    
  def clearSele(): OnContextUnit =
    context.selections.clear()

  def resetData(dataS: List[GridValueExport[String]]): OnContextUnit =
    context.grid.resetData(dataS)
    draw()
  def draw(): OnContextUnit =
    context.gc.clearRect(0, 0, context.canvas.width, context.canvas.height)
    context.grid.gridValues().foreach { case Positioned(x, y, color) =>
      drawPoint(x, y, color)

  }



  def view(): HTMLElement =
    inline val cntColorStep = 500
    inline val colorNum = 255 * 255 * 255
    inline val colrStep = 360d / 500d

    val gridX = 40
    val gridY = 40
    var grid = Grid[String](gridX, gridY)
    val fact = 10
    val myCanvas: HTMLCanvasElement = canvas
    given Context = new Context(
      grid,
      myCanvas,
      tillColor(),
      fact,
      Color.RGB(0, 0, 0),
      DrawProcessEvent,
      NoActionParam,Nil
    )
    val cWidth = gridX * fact
    val cHeight = gridY * fact
    var mousedown = false
    def arc: Seq[(Double)] = (for {
      c <- 0 until 500

    } yield c) map (_ * colrStep)

    def colorLine() = div(
      childs(
        arc
          .map { (c) =>

            val s = s"hsl(${c} 100% 50%)"

            s
          }
          .map { cssClor =>
            val spanc = span
            spanc.style.backgroundColor = cssClor
            spanc.style.width = "1px"
            spanc.style.height = "15px"
            spanc.style.display = "inline-block"
            spanc
          }
      )
    )

    def colorLineCanvas(iniH : Double,
        posConsumer: Double => Unit
    ): (VarValueDouble, HTMLElement) =
      val varval = VarValue(iniH)
      val cursor = div(_class("cursor")) 
      cursor.style.top = "2px"
      cursor.style.left = s"${(iniH/colrStep)-5}px"
      cursor.style.pointerEvents = "none"
      val canvasp = canvas(me(can =>
        can.width = cntColorStep
        can.height = 15
        import bon.jo.HtmlEvent.onmousedownandmove

        can.onmousedownandmove { e =>
          
          given HTMLElement = can
          val xx = xme(e)
          val nH = xx * colrStep
          cursor.style.left = s"${xx-5}px"
          varval.value = nH
          posConsumer(nH)

        }
        val context =
          can.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

        arc
          .map { (c) =>

            val s = s"hsl(${c} 100% 50%)"

            s
          }
          .zipWithIndex
          .foreach { (cssClor, i) =>
            context.beginPath()
            context.fillStyle = cssClor
            context.rect(i, 0, 1, 15)
            context.fill()
            context.closePath()
          }
      ))
      val ret = div(childs(canvasp,cursor))
      ret.classList.add("relative-inline-block")

      (varval, ret)

    inline def hslColor100_50(h: Double): String = s"hsl(${h} 100% 50%)"
    inline def hslColor(h: Double, s: Double, l: Double): String =
      s"hsl(${h} ${s}% ${l}%)"



    
    
    val saveColorDiv = div(_class("color-saved"))
    val buttonSaveColor= button(_text("save color"), click(_ => saveColor(saveColorDiv)))
    
    val buttonColorFromDraw =
      button(_text("get all colors"), click(_ => colorFromDraw(saveColorDiv)))
    val rPicker = input(me(_.style.width = "30px"))
    val gPicker = input(me(_.style.width = "30px"))
    val bPicker = input(me(_.style.width = "30px"))
    val colPi = div(childs(context.colorPicker,buttonSaveColor,buttonColorFromDraw,saveColorDiv, div(childs(rPicker, gPicker, bPicker))))

    def hex(c: Color.RGB) =
      String.format("#%02x%02x%02x", c.r, c.g, c.b)


    def colorChage(): OnContextUnit =
      updateColor(
        Color.RGB(rPicker.value.toInt, gPicker.value.toInt, bPicker.value.toInt)
      )

    rPicker.onchange = { _ =>
      colorChage()
    }
    gPicker.onchange = { _ =>
      colorChage()
    }
    bPicker.onchange = { _ =>
      colorChage()
    }
    prepare(myCanvas, cWidth, cHeight)

    given (String => HTMLElement) = i => div(_text(i.toString))
    given PaletteContext = PaletteContext("tool-select", "row", "cell")
    val palette = Palette(2,true, "Draw", "Select","Paste")

    palette.listen = {
      case "Draw" =>
        context.actionParam = NoActionParam
        context.currentProcess = DrawProcessEvent
      case "Select" =>
        context.currentProcess = SelectProcessEvent
      case "Paste" =>
        context.currentProcess = PasteProcessEvent
    }
    palette.select(0)
    val parentCanvas = div(_class("parent-c"), childs(myCanvas))
    parentCanvas.onmousemove = e =>
      if mousedown then context.currentProcess.process.tupled(xyInGrid(e))
    EventAdder.touchmove[TouchEvent]( e =>
      console.log(e)
      if mousedown then context.currentProcess.process.tupled(xyInGrid(e))
    )(using parentCanvas)
    parentCanvas.onmousedown = e =>
      mousedown = true
      context.currentProcess.start.tupled(xyInGrid(e))
    EventAdder.touchstart[TouchEvent]( e =>
      mousedown = true
      println("touche start")
      console.log(e)
      context.currentProcess.start.tupled(xyInGrid(e))
    )(using parentCanvas)
    EventAdder.touchend[TouchEvent]( e =>
      mousedown = false
      context.currentProcess.end.tupled(xyInGrid(e))
    )(using parentCanvas)
    parentCanvas.onmouseup = e =>
      mousedown = false
      context.currentProcess.end.tupled(xyInGrid(e))
    EventAdder.click[MouseEvent](e => context.currentProcess.process.tupled(xyInGrid(e)))(using parentCanvas)
    val buttonDeleteSel = button(_text("Delete selections"), click(_ => deleteSele()))
    val buttonRemoveAllSel =
      button(_text("Clear selections"), click(_ => clearSele()))
    val buttonCopySel =
      button(_text("Copy selections"), click(_ => copySel()))
    val buttonApplyColToSelElements =
      button(_text("Apply color"), click(_ => applyCuurentColorToSel()))
    val strokeButton =
      button(_text("Stroke selection"), click(_ => strokeSel()))
    val fillButton =
      button(_text("Fill selection"), click(_ => fillSel()))
    def selectionPalette():Palette[HTMLElement] =
      given (String => HTMLElement) = i => div(_text(i.toString))
      given PaletteContext = PaletteContext("select-action-select", "row", "cell-large")
      Palette(4,false,buttonDeleteSel,buttonCopySel,buttonApplyColToSelElements, buttonRemoveAllSel, strokeButton, fillButton)


    val ret = div(
      childs(
        parentCanvas,
        div(childs(selectionPalette().root)),
        palette.root,
        colPi
      )
    )

    def save(): OnHTMLElement =
      val s = grid.json().toJsonString()
      val aLink = a(
        _text("Download"),
        me(_.asInstanceOf[scalajs.js.Dynamic].download = "image.json"),
        me(_.href = s"""data:application/json;base64,${Base64.getEncoder
          .encodeToString(s.getBytes)}""")
      )
      OnHtml().append(div(childs(aLink)))

    val buttonSave = button(_text("save"), click(save()(using ret)))

    val i = input(
      me(_.`type` = "file"),
      me(_.name = "img-json"),
      me(_.id = "img-json"),
      me(_.style.display = "none")
    )
    i.onchange = e => {
      val f: FileReader = new FileReader()
      val tRef = (System.currentTimeMillis)
      f.onloadend = l =>
        println("onloadend"+(System.currentTimeMillis - tRef))
        val res = f.result.toString
        val resParsed = JSON.parse(res)
        val dataS: List[GridValueExport[String]] = resParsed.asInstanceOf[scalajs.js.Array[scalajs.js.Dynamic]].map{ e =>
          GridValueExport(e.v.asInstanceOf,e.xy.asInstanceOf) 
        }.toList
        resetData(dataS)
        println("reset data"+(System.currentTimeMillis - tRef))
      
      f.readAsText(i.files(0), "utf-8")
    }
    inline def open(): HTMLDivElement = div(
      childs(
        button(childs(label(_text("Open"), me(_.htmlFor = "img-json")), i))
      )
    )



    lazy val (vRed: VarValueDouble, cRed) = Cursor(0.2,
      f =>
        updateColor(
          Color.RGB(
            (f * 255).round.toInt,
            (vGreen.value * 255).round.toInt,
            (vBlue.value * 255).round.toInt
          )
        ),
      ret
    )
    lazy val (vGreen: VarValueDouble, cGreen) = Cursor(0.5,
      f =>
        updateColor(
          Color.RGB(
            (vRed.value * 255).round.toInt,
            (f * 255).round.toInt,
            (vBlue.value * 255).round.toInt
          )
        ),
      ret
    )
    lazy val (vBlue: VarValueDouble, cBlues) = Cursor(0.7,
      f =>
        updateColor(
          Color.RGB(
            (vRed.value * 255).round.toInt,
            (vGreen.value * 255).round.toInt,
            (f * 255).round.toInt
          )
        ),
      ret
    )

    lazy val (vS: VarValueDouble, cS) = Cursor(1,
      s => updateColor(Color.HSL(vH.value, s * 100, vL.value * 100)),
      ret
    )
    lazy val (vL: VarValueDouble, cL) = Cursor(0.5,
      l => updateColor(Color.HSL(vH.value, vS.value * 100, l * 100)),
      ret
    )
    lazy val (vH: VarValueDouble, cH) = colorLineCanvas(120,h =>
      updateColor(Color.HSL(h, vS.value * 100, vL.value * 100))
    )
    ret.append(
      div(childs(span(_text("R : ")),cRed)),
      div(childs(span(_text("G : ")),cGreen)),
      div(childs(span(_text("B : ")),cBlues)),
      div(childs(span(_text("H : ")),cH)),
      div(childs(span(_text("S : ")),cS)),
      div(childs(span(_text("L : ")),cL)),
      div(childs(buttonSave)),
      open()
    )
    updateColor(Color.HSL(vH.value, vS.value * 100, vL.value * 100))
    ret
