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
    val xVal = x
    el.style.left = xVal+"px"
    xVal

  def x(using ev : MouseEvent,c : HTMLElement ) = ev.pageX - c.offsetLeft - 5
  def y(using ev : MouseEvent,c : HTMLElement ) = ev.pageY - c.offsetTop

  def apply(event : Double => Unit,moseHandleMove : HTMLElement):(VarValueDouble,HTMLElement) =

    val varValue = VarValue(0d)
    val cursor = div(me(_.style.position="absolute")) 
    cursor.style.left = "0px"
    cursor.style.backgroundColor = "white"
    cursor.style.width = "10px"
    cursor.style.height = "10px"
   
    val content = div(childs(cursor),me(_.style.position="relative"))
    content.style.width = "100px"
    content.style.height = "10px"
    content.style.backgroundColor = "black"
    var mousDown = false
    cursor.addEventListener[MouseEvent]("mousedown",_ => mousDown = true)
    moseHandleMove.addEventListener[MouseEvent]("mouseup", _ => mousDown = false)
    moseHandleMove.addEventListener[MouseEvent]("mousemove", e => {
      if mousDown then 
        val value = draw(e,cursor,content)/100
        event(value )
        varValue.value = value
    })
    (varValue,content)
