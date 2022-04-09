package bon.jo.home

import org.scalajs.dom.HTMLElement
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.HTMLHtmlElement
object Cursor:
  def draw(ev : MouseEvent,el : HTMLElement,content : HTMLElement) :Double=
    given MouseEvent = ev
    given HTMLElement = content
    val xVal = x.round.toInt
    draw(xVal,el)
    xVal

  def draw(xVal : Int,el : HTMLElement) :Unit=
    
    el.style.left = (xVal-5)+"px"
    

  def x(using ev : MouseEvent,c : HTMLElement ) = ev.pageX - c.offsetLeft - 5
  def y(using ev : MouseEvent,c : HTMLElement ) = ev.pageY - c.offsetTop

  def apply(ini : Double,event : Double => Unit,moseHandleMove : HTMLElement):(VarValueDouble,HTMLElement) =

    val varValue = VarValue(ini)
    val cursor = div(_class("cursor")) 
    
    val content = div(childs(cursor),me(_.style.position="relative"))
    content.style.width = "100px"
  
    content.classList.add("relative-inline-block")
    content.classList.add("cursor-div")
    var mousDown = false
    content.addEventListener[MouseEvent]("mousedown",_ => mousDown = true)
    moseHandleMove.addEventListener[MouseEvent]("mouseup", _ => mousDown = false)
    moseHandleMove.addEventListener[MouseEvent]("mousemove", e => {
      if mousDown then 
        val value = draw(e,cursor,content)/100
        event(value )
        varValue.value = value
    })
    draw((ini*100).round.toInt,cursor)
    (varValue,content)
