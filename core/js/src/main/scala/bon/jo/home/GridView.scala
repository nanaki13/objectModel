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
import bon.jo.Draw.EmptyGridElement
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
    def process(e: MouseEvent): OnContextUnit
    def start(e: MouseEvent): OnContextUnit
    def end(e: MouseEvent): OnContextUnit
  object DrawProcessEvent extends ProcessEvent:
    def process(e: MouseEvent): OnContextUnit = draw(e)
    def start(e: MouseEvent): OnContextUnit = ()
    def end(e: MouseEvent): OnContextUnit = ()
  object SelectProcessEvent extends ProcessEvent:
    def process(e: MouseEvent): OnContextUnit = selectAction(e)
    def start(e: MouseEvent): OnContextUnit =
      context.actionParam =
        SelectActionParam(true, div(_class("select-rect")), 0, 0)
    def end(e: MouseEvent): OnContextUnit =
      given MouseEvent = e
      given HTMLElement = context.parentCanvas
      val param = context.actionParam.asInstanceOf[SelectActionParam]
      context.selections.add(
        RectSelect(param.xIni, param.yIni, x, y),
        param.selectDiv
      )
  class Context(
      val grid: Grid[String],
      val c: HTMLCanvasElement,
      var factor: Double,
      var color: Color,
      var currentProcess: ProcessEvent,
      var actionParam: ActionParam,
      var savedColor : List[Color]
  ):
    val gc = c.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    def parentCanvas = c.parentElement
    object selections:
      var select: List[(Sel, HTMLElement)] = Nil
      def add(sel: Sel, el: HTMLElement) =
        select = select :+ sel -> el
      def clear() =
        select.foreach { (_, e) =>
          parentCanvas.removeChild(e)
        }
        select = Nil
    def xInGrid(using ev: MouseEvent): Int = (x / factor).toInt
    def yInGrid(using ev: MouseEvent): Int = (y / factor).toInt
    def xInGrid(x: Int): Int = (x / factor).toInt
    def yInGrid(y: Int): Int = (y / factor).toInt
  object Context:
    type OnContext[A] = Context ?=> A
    type OnContextUnit = OnContext[Unit]
    inline def apply(): OnContext[Context] = summon

  def x(using ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetX.asInstanceOf[Int]
  def y(using ev: MouseEvent): Int =
    ev.asInstanceOf[scalajs.js.Dynamic].offsetY.asInstanceOf[Int]
  inline def context: OnContext[Context] = Context()
  def selectAction(ev: MouseEvent): OnContextUnit = {
    println("selectAction")
    val param: SelectActionParam = context.actionParam.asInstanceOf
    given MouseEvent = ev
    given HTMLElement = context.c
    println((x, y))
    if param.begin then
      param.begin = false
      param.selectDiv.style.left = s"${x}px"
      param.selectDiv.style.top = s"${y}px"
      param.xIni = x
      param.yIni = y
      context.parentCanvas.append(param.selectDiv)
    else
      var wR = x - param.xIni
      var hR = y - param.yIni
      hR = if hR < 0 then
        param.selectDiv.style.top = s"${param.yIni + hR}px"
        -hR
      else
        param.selectDiv.style.top = s"${param.yIni}px"
        hR
      wR = if wR < 0 then
        param.selectDiv.style.left = s"${param.xIni + wR}px"
        -wR
      else
        param.selectDiv.style.left = s"${param.xIni}px"
        wR

      param.selectDiv.style.width = s"${wR}px"
      param.selectDiv.style.height = s"${hR}px"

  }
  def draw(ev: MouseEvent): OnContextUnit =
    given MouseEvent = ev
    given HTMLElement = context.c
    val fact = context.factor
    val grid = context.grid
    val colot = context.color
    val x_ = context.xInGrid
    val y_ = context.yInGrid
    grid(x_, y_) = GridValue(colot.toString)
    drawPoint(x_, y_, colot.toString)

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
    override def toString(): String =
      this match
        case RGB(r, g, b) => s"rgb($r,$g,$b)"
        case HSL(h, s, l) => s"hsl(${h} ${s}% ${l}%)"

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

    def colorLineCanvas(
        posConsumer: Double => Unit
    ): (VarValueDouble, HTMLCanvasElement) =
      val varval = VarValue(0d)
      val canvasp = canvas(me(can =>
        can.width = cntColorStep
        can.height = 15
        import bon.jo.HtmlEvent.onmousedownandmove
        def x(using ev: MouseEvent, c: HTMLElement) = ev.pageX - c.offsetLeft
        def y(using ev: MouseEvent, c: HTMLElement) = ev.pageY - c.offsetTop
        can.onmousedownandmove { e =>
          given MouseEvent = e
          given HTMLElement = can
          val nH = x * colrStep
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
      (varval, canvasp)

    inline def hslColor100_50(h: Double): String = s"hsl(${h} 100% 50%)"
    inline def hslColor(h: Double, s: Double, l: Double): String =
      s"hsl(${h} ${s}% ${l}%)"

    val colorPicker = tillColor()
    def updateColor(c: Color): OnContextUnit =
      context.color = c
      colorPicker.style.backgroundColor = c.toString
    def saveColor(out : HTMLElement): OnContextUnit = 
      val saved = context.color
      context.savedColor :+ saved
      val d = tillColor()
      out.append(d)
      d.style.backgroundColor = saved.toString
      EventAdder.click(_ => 
       updateColor(saved)
      )(using d)

    inline def tillColor() =  div(me(_.style.width = "30px"), me(_.style.height = "30px"))
    
    val saveColorDiv = div
    val buttonSaveColor= button(_text("save color"), click(_ => saveColor(saveColorDiv)))
    val rPicker = input(me(_.style.width = "30px"))
    val gPicker = input(me(_.style.width = "30px"))
    val bPicker = input(me(_.style.width = "30px"))
    val colPi = div(childs(colorPicker,buttonSaveColor,saveColorDiv, div(childs(rPicker, gPicker, bPicker))))

    def hex(c: Color.RGB) =
      String.format("#%02x%02x%02x", c.r, c.g, c.b)

    


    def deleteSele(): OnContextUnit =
      context.selections.select.foreach {
        case (RectSelect(xi, yi, xe, ye), _) =>
          val xMin = context.xInGrid(Math.min(xi, xe))
          val yMin = context.yInGrid(Math.min(yi, ye))
          val xMax = context.xInGrid(Math.max(xi, xe)) + 1
          val yMax = context.yInGrid(Math.max(yi, ye)) + 1
          for {
            xx <- xMin to xMax
            yy <- yMin to yMax
          } {
            context.grid(xx, yy) = EmptyGridElement
          }
          context.gc.clearRect(
            xMin * context.factor,
            yMin * context.factor,
            (xMax - xMin) * context.factor,
            (yMax - yMin) * context.factor
          )
      }
    def clearSele(): OnContextUnit =
      context.selections.clear()



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
    val palette = Palette(2, "Draw", "Select")

    palette.listen = {
      case "Draw" =>
        context.actionParam = NoActionParam
        context.currentProcess = DrawProcessEvent
      case "Select" =>
        context.currentProcess = SelectProcessEvent
    }
    palette.select(0)
    val parentCanvas = div(_class("parent-c"), childs(myCanvas))
    parentCanvas.onmousemove = e =>
      if mousedown then context.currentProcess.process(e)
    parentCanvas.onmousedown = e =>
      mousedown = true
      context.currentProcess.start(e)
    parentCanvas.onmouseup = e =>
      mousedown = false
      context.currentProcess.end(e)
    EventAdder.click(e => context.currentProcess.process(e))(using parentCanvas)
    val buttonp = button(_text("delete selections"), click(_ => deleteSele()))
    val buttonRemoveAllSel =
      button(_text("clear selections"), click(_ => clearSele()))

    val ret = div(
      childs(
        parentCanvas,
        div(childs(buttonp, buttonRemoveAllSel)),
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
      f.onloadend = l =>
        val dataJson: All.ListAll[String] = All(f.result.toString).asInstanceOf
        val dataS: List[GridValueExport[String]] = dataJson.value.map(
          _.asInstanceOf[All.ObjectAll[String]].toGridValueExport
        )
        resetData(dataS)

      f.readAsText(i.files(0), "utf-8")
    }
    inline def open(): HTMLDivElement = div(
      childs(
        button(childs(label(_text("Open"), me(_.htmlFor = "img-json")), i))
      )
    )

    def resetData(dataS: List[GridValueExport[String]]): OnContextUnit =
      context.gc.clearRect(0, 0, cWidth, cHeight)
      grid.resetData(dataS)
      grid.gridValues().foreach { case Positioned(x, y, color) =>
        drawPoint(x, y, color)

      }

    lazy val (vRed: VarValueDouble, cRed) = Cursor(
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
    lazy val (vGreen: VarValueDouble, cGreen) = Cursor(
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
    lazy val (vBlue: VarValueDouble, cBlues) = Cursor(
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

    lazy val (vS: VarValueDouble, cS) = Cursor(
      s => updateColor(Color.HSL(vH.value, s * 100, vL.value * 100)),
      ret
    )
    lazy val (vL: VarValueDouble, cL) = Cursor(
      l => updateColor(Color.HSL(vH.value, vS.value * 100, l * 100)),
      ret
    )
    lazy val (vH: VarValueDouble, cH) = colorLineCanvas(h =>
      updateColor(Color.HSL(h, vS.value * 100, vL.value * 100))
    )
    ret.append(
      cRed,
      cGreen,
      cBlues,
      cH,
      cS,
      cL,
      div(childs(buttonSave)),
      open()
    )
    ret
