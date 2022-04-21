package bon.jo.home
import bon.jo.home.GridViewContext
import bon.jo.home.GridViewContext.*
import org.scalajs.dom.TouchEvent
import org.scalajs.dom.HTMLElement
import bon.jo.home.ProcessEvent.SelectActionParam
import bon.jo.Draw.Positioned
import bon.jo.home.GridView.RectSelect
import bon.jo.Draw.GridValue
import bon.jo.Draw.EmptyGridElement
import bon.jo.Draw.Grid
import bon.jo.Draw.Access
import bon.jo.Draw.AccessVar
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import bon.jo.HtmlEvent.events
import bon.jo.Draw.Moving
import bon.jo.Draw
import bon.jo.utils.ModelAndView
import bon.jo.home.OnContextSelectActions.Bound
import bon.jo.Draw.FrameGrid
import org.scalajs.dom.HTMLInputElement
object OnContextSelectActions:
  case class Bound(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
    inline def width = xMax - xMin
    inline def height = yMax - yMin
trait OnContextSelectActions {
  self: OnGridViewContext =>

  def selectAction(xI: Int, yI: Int): OnContextUnit = {

    val param: SelectActionParam = context.actionParam.asInstanceOf

    given HTMLElement = context.canvas
    if param.begin then
      param.begin = false
      param.selectDiv.style.left = s"${xI * context.factor}px"
      param.selectDiv.style.top = s"${yI * context.factor}px"
      param.xIni = xI
      param.yIni = yI
      context.parentCanvas.append(param.selectDiv)
    else
      var wR = xI - param.xIni
      var hR = yI - param.yIni
      hR = if hR < 0 then
        param.selectDiv.style.top = s"${(param.yIni + hR) * context.factor}px"
        -hR
      else
        param.selectDiv.style.top = s"${param.yIni * context.factor}px"
        hR + 1
      wR = if wR < 0 then
        param.selectDiv.style.left = s"${(param.xIni + wR) * context.factor}px"
        -wR
      else
        param.selectDiv.style.left = s"${param.xIni * context.factor}px"
        wR + 1

      param.selectDiv.style.width = s"${wR * context.factor}px"
      param.selectDiv.style.height = s"${hR * context.factor}px"

  }
  def copySel(): OnContextUnit =
    context.gridsCopy = Nil
    doOnSelctedcoords(
      (xx, yy) =>
        context.grid(xx, yy) match
          case value: GridValue[_] =>
            val g = context.gridsCopy.head
            g.v(xx - g.x, yy - g.y) = value
          case o =>
      ,
      (xMin: Int, xMax: Int, yMin: Int, yMax: Int) => {
        context.gridsCopy = Positioned(
          xMin,
          yMin,
          Grid(xMax - xMin, yMax - yMin)
        ) :: context.gridsCopy
      }
    )
  def paste(x: Int, y: Int): OnContextUnit =
    context.gridsCopy.foreach { pos =>
      pos.v.gridValues().foreach { gvPos =>
        val xx = x + gvPos.x
        val yy = y + gvPos.y
        if context.grid.isInGrid(xx, yy) then context.grid(xx, yy) = gvPos.v
      }
    }
    draw()

  def applyCuurentColorToSel(): OnContextUnit =
    doOnSelctedcoords(
      (xx, yy) =>
        context.grid(xx, yy) match
          case value: GridValue[_] =>
            updateGridAndDraw(xx, yy)
          case o =>
      ,
      (_, _, _, _) => ()
    )
  def strokeSel(): OnContextUnit =
    context.selections.select.foreach { case (RectSelect(xi, yi, xe, ye), _) =>
      val xMin = Math.min(xi, xe)
      val yMin = Math.min(yi, ye)
      val xMax = Math.max(xi, xe)
      val yMax = Math.max(yi, ye)
      for {
        xx <- xMin until xMax
      } {

        updateGridAndDraw(xx, yMin)
        updateGridAndDraw(xx, yMax - 1)
      }
      for {
        yy <- yMin until yMax
      } {

        updateGridAndDraw(xMin, yy)
        updateGridAndDraw(xMax - 1, yy)
      }
    }
  def fillSel(): OnContextUnit =
    println("fillSel start = " + context.grid.data.count(_ != EmptyGridElement))
    context.selections.select.foreach { case (RectSelect(xi, yi, xe, ye), _) =>
      val xMin = Math.min(xi, xe)
      val yMin = Math.min(yi, ye)
      val xMax = Math.max(xi, xe)
      val yMax = Math.max(yi, ye)
      for {
        xx <- xMin until xMax
        yy <- yMin until yMax
      } yield {

        updateGridAndDraw(xx, yy)
      }
    }
    println("fillSel end = " + context.grid.data.count(_ != EmptyGridElement))

  def addSheet(
      p: Positioned[Grid[String]] with Access with AccessVar with Moving[String]
  ): OnContextUnit =
    val sheetDiv = div
    val mv: SheetV = ModelAndView(p, sheetDiv)
    sheetDiv.classList.add("sheet-rect")
    mv.redraw()
    context.parentCanvas.append(sheetDiv)
    context.grid.sheet = p :: context.grid.sheet
    context.sheetsMv = mv :: context.sheetsMv
    val seeCheck = input(me(_.`type` = "checkbox"), me(_.checked = true))
    val lockCheck = input(me(_.`type` = "checkbox"), me(_.checked = false))
    val lastCheck: VarValue[Option[HTMLInputElement]] = VarValue(None)
    val framesDiv: VarValue[Option[HTMLElement]] = VarValue(None)

    val delete = button(_text("delete"))
    val moveString = input
    val addFrame = button(_text("add frame"))

    val sheetViewDiv = div(
      _text("sheet : " + context.grid.sheet.size),
      childs(
        span(_text("see:")),
        seeCheck,
        span(_text("lock:")),
        lockCheck,
        span(_text("movment")),
        moveString,
        addFrame,
        delete
      )
    )
    def getFramesDiv() = framesDiv.get{
      val fDiv = div
      sheetViewDiv.append(fDiv)
      fDiv
    }
    p.v match
      case e: FrameGrid[_] =>
        e.frames.zipWithIndex.foreach((_, i) =>
          addFrameInput(i, e, lastCheck,  getFramesDiv())
        )
        e
      case o =>
    seeCheck.events.change(_ =>
      p.canRead = seeCheck.checked
      draw()
    )
    moveString.events.change(_ =>
      p.moveString(moveString.value, context.grid.xSize, context.grid.ySize)
    )
    lockCheck.events.change(_ => p.canWrite = !lockCheck.checked)
    val ev = sheetViewDiv.events
    ev.mouseenter(e => sheetDiv.classList.add("focus"))
    ev.mouseleave(e => sheetDiv.classList.remove("focus"))
    delete.events.click(_ =>
      context.grid.sheet = context.grid.sheet.filter(_ != p)
      context.sheetsMv = context.sheetsMv.filter(_ != mv)
      context.parentCanvas.removeChild(sheetDiv)
      context.sheetViewsDiv.removeChild(sheetViewDiv)
      draw()
    )

    addFrame.events.click(f => {
      val framep: FrameGrid[String] = p.v match
        case e: FrameGrid[_] =>
          e.addFrame()
          e
        case o =>
          val f = FrameGrid[String](p.v.xSize, p.v.ySize)
          val selectFrameInput = input(me(_.`type` = "checkbox"))
          addFrameInput(0, f, lastCheck,  getFramesDiv())
          p.v = f
          val d = p.v.exportFun().toList
          p.v.resetData(d)
          f.addFrame()
          f
      addFrameInput(framep.frames.size - 1, framep, lastCheck,  getFramesDiv())

    })
    context.sheetViewsDiv.append(sheetViewDiv)

  def addFrameInput(
      idxFrame: Int,
      framep: FrameGrid[String],
      lastCheck: VarValue[Option[HTMLInputElement]],
      framesDiv: HTMLElement
  ): OnContextUnit =

    framep.currentFrame = idxFrame
    lastCheck.value.foreach(_.checked = false)
    val selectFrameInput =
      input(me(_.`type` = "checkbox"), me(_.checked = true),_class("selectFrameInput"))
    lastCheck.value = Some(selectFrameInput)
    val deleteButton = button(_text("x"))
    val frameView =  div(childs(span(_text(idxFrame.toString),_class("selectFrameInputLabel")), selectFrameInput,deleteButton))
    def currentIdxFrame = framesDiv.children.indexWhere(_ == frameView)
    selectFrameInput.events.change { _ =>
      if selectFrameInput.checked then
        lastCheck.value.foreach(_.checked = false)
        lastCheck.value = Some(selectFrameInput)
        framep.currentFrame = currentIdxFrame
        draw()
      else if selectFrameInput == lastCheck.value.get then
        selectFrameInput.checked = true
    }
    
    deleteButton.events.click(_ => 
      val idx = currentIdxFrame
      framep.frames = framep.frames.slice(0,idx) ++ framep.frames.slice(idx+1,framep.frames.size)
      println(framep.frames)
      framesDiv.removeChild(frameView)
      if framep.currentFrame >= idx then   
        framep.currentFrame = framep.currentFrame -1
        framesDiv.children.zipWithIndex.foreach{ (e,i) =>
          val in = e.getElementsByClassName("selectFrameInput")(0).asInstanceOf[HTMLInputElement]
          in.checked = framep.currentFrame == i
          if framep.currentFrame == i then
            lastCheck.value = Some(in)
          e.getElementsByClassName("selectFrameInputLabel")(0).textContent = i.toString
        }
    )
    framesDiv.append(frameView)
    
  def sheetFromSel(): OnContextUnit =
    selectedBound().foreach { b =>
      val p = new Positioned(b.xMin, b.yMin, Grid[String](b.width, b.height))
        with Access
        with AccessVar
        with Moving[String]
      addSheet(p)

    }

  def selectedBound(): OnContext[Iterable[Bound]] =
    context.selections.selectedBound()

  def doOnSelctedcoords[A](
      f: (Int, Int) => A,
      sel: (xMin: Int, xMax: Int, yMin: Int, yMax: Int) => Unit
  ): OnContext[List[Seq[A]]] =
    context.selections.select.map { case (RectSelect(xi, yi, xe, ye), _) =>
      val xMin = Math.min(xi, xe)
      val yMin = Math.min(yi, ye)
      val xMax = Math.max(xi, xe)
      val yMax = Math.max(yi, ye)
      sel(xMin, xMax, yMin, yMax)
      for {
        xx <- xMin until xMax
        yy <- yMin until yMax
      } yield {
        f(xx, yy)
      }
    }
  def deleteSele(): OnContextUnit =
    doOnSelctedcoords(
      context.grid(_, _) = EmptyGridElement,
      (xMin: Int, xMax: Int, yMin: Int, yMax: Int) => {
        context.gc.clearRect(
          xMin * context.factor,
          yMin * context.factor,
          (xMax - xMin) * context.factor,
          (yMax - yMin) * context.factor
        )
      }
    )

  def clearSele(): OnContextUnit =
    context.selections.clear()
}
