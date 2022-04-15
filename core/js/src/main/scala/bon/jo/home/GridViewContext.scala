package bon.jo.home

import bon.jo.Draw.MasterGrid
import bon.jo.Draw.Grid
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.HTMLElement
import bon.jo.home.Color
import bon.jo.home.ProcessEvent
import bon.jo.home.ProcessEvent.ActionParam
import bon.jo.Draw.Positioned
import org.scalajs.dom.CanvasRenderingContext2D
import bon.jo.home.GridView.Sel
import bon.jo.Draw
import bon.jo.home.OnContextSelectActions.Bound
import bon.jo.home.GridView.RectSelect
import bon.jo.home.GridViewContext.Factor
import bon.jo.home.GridViewContext.BaseDraw

  class GridViewContext(
      var grid: MasterGrid[String],
      val canvas: HTMLCanvasElement,
      val colorPicker: HTMLElement,
      val sheetViewsDiv: HTMLElement,
      var factor: Int,
      var color: Color,
      var currentProcess: ProcessEvent,
      var actionParam: ActionParam,
      var savedColor : List[Color],
      
      var gridsCopy :List[Positioned[Grid[String]]] = Nil,
      var sheetsMv:List[GridViewContext.SheetV] = Nil
  ) extends Factor with BaseDraw:
    val gc : CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    def parentCanvas = canvas.parentElement
    object selections:
      var select: List[(Sel, HTMLElement)] = Nil
      def selectedBound():Iterable[Bound] = 
        select.map {
        case (RectSelect(xi, yi, xe, ye), _) =>
            val xMin = Math.min(xi, xe)
            val yMin = Math.min(yi, ye)
            val xMax = Math.max(xi, xe) 
            val yMax = Math.max(yi, ye) 
            Bound(xMin,xMax,yMin,yMax)
            }  
      def selectedBoundAndHtml():Iterable[(Bound,HTMLElement)] = 
        select.map {
        case (RectSelect(xi, yi, xe, ye), v) =>
            val xMin = Math.min(xi, xe)
            val yMin = Math.min(yi, ye)
            val xMax = Math.max(xi, xe) 
            val yMax = Math.max(yi, ye) 
            ( Bound(xMin,xMax,yMin,yMax),v)
        } 
      
      def add(sel: Sel, el: HTMLElement) =
        select = select :+ sel -> el
      def clear() =
        select.foreach { (_, e) =>
          parentCanvas.removeChild(e)
        }
        select = Nil
      def redraw():GridViewContext.OnContextUnit =
        selectedBoundAndHtml().foreach{
        (b,h)=>
          h.style.left = s"${b.xMin * GridViewContext.context.factor}px"
          h.style.top = s"${b.yMin * GridViewContext.context.factor}px"
          h.style.width = s"${b.width * GridViewContext.context.factor}px"
          h.style.height = s"${b.height * GridViewContext.context.factor}px"
        }  


  object GridViewContext:
    type OnContext[A] = GridViewContext ?=> A
    type OnFactor[A] = Factor ?=> A
    type OnBaseDraw[A] = BaseDraw ?=> A
    type OnBaseDrawFactor[A] = BaseDraw & Factor ?=> A
    type OnContextUnit = OnContext[Unit]
    inline def apply(): OnContext[GridViewContext] = summon
    inline def context: OnContext[GridViewContext] = GridViewContext()
    inline def factor: OnFactor[Int] = summon.factor
    inline def gc: OnBaseDraw[CanvasRenderingContext2D] = summon.gc
    inline def color: OnBaseDraw[Color] = summon.color
    inline def grid: OnBaseDraw[MasterGrid[String]] = summon.grid
    type SheetV = Draw.SheetMV[String,HTMLElement]
    trait Factor:
      var factor: Int
    trait BaseDraw:
      val gc : CanvasRenderingContext2D 
      var color: Color
      var grid: MasterGrid[String]
