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

trait OnContextSelectActions {
  self : OnGridViewContext =>
  
    def selectAction(xI : Int,yI : Int): OnContextUnit = {

      val param: SelectActionParam = context.actionParam.asInstanceOf

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
        context.grid(xx,yy) match
          case value :  GridValue[_] => 
              val g = context.gridsCopy.head
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
}