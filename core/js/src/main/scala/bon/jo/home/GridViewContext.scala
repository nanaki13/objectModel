package bon.jo.home

import bon.jo.Draw.Grid
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.HTMLElement
import bon.jo.home.Color
import bon.jo.home.ProcessEvent
import bon.jo.home.ProcessEvent.ActionParam
import bon.jo.Draw.Positioned
import org.scalajs.dom.CanvasRenderingContext2D
import bon.jo.home.GridView.Sel

  class GridViewContext(
      var grid: Grid[String],
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

  object GridViewContext:
    type OnContext[A] = GridViewContext ?=> A
    type OnContextUnit = OnContext[Unit]
    inline def apply(): OnContext[GridViewContext] = summon
    inline def context: OnContext[GridViewContext] = GridViewContext()