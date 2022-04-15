package bon.jo.home
import bon.jo.home.GridViewContext.*
import org.scalajs.dom.HTMLElement
import bon.jo.home.GridView.RectSelect
import bon.jo.HtmlPredef.*
import bon.jo.MiniDsl.*
import bon.jo.Draw.EmptyGridElement
trait ProcessEvent:
    def process(xI : Int,yI:Int): OnContextUnit
    def start(xI : Int,yI:Int): OnContextUnit
    def end(xI : Int,yI:Int): OnContextUnit
object ProcessEvent:
  trait ActionParam
  case class SelectActionParam(
      var begin: Boolean,
      selectDiv: HTMLElement,
      var xIni: Int,
      var yIni: Int
  ) extends ActionParam
  object NoActionParam extends ActionParam

  trait GridViewProcessEvent:
    self: OnGridViewContext with OnContextSelectActions=>
      object DrawProcessEvent extends ProcessEvent:
        def process(xI : Int,yI:Int): OnContextUnit = draw(xI, yI)
        def start(xI : Int,yI:Int): OnContextUnit = ()
        def end(xI : Int,yI:Int): OnContextUnit = ()
      object EraseProcessEvent extends ProcessEvent:
        def process(xI : Int,yI:Int): OnContextUnit = delete(xI, yI)
        def start(xI : Int,yI:Int): OnContextUnit = ()
        def end(xI : Int,yI:Int): OnContextUnit = ()
      object PasteProcessEvent extends ProcessEvent:
        def process(xI : Int,yI:Int): OnContextUnit =      
          println("paste size b = "+context.grid.data.count(_ != EmptyGridElement))
          paste(xI,yI)
          println("paste size a = "+context.grid.data.count(_ != EmptyGridElement))
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
          val xx = if xI - param.xIni >= 0 then xI+1 else xI
          val yy = if yI - param.yIni >= 0 then yI+1 else yI
          context.selections.add(
            RectSelect(param.xIni, param.yIni, xx, yy),
            param.selectDiv
          )
