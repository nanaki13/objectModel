package bon.jo.home

import org.scalajs.dom.HTMLElement
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import org.scalajs.dom.MouseEvent
import bon.jo.HtmlEvent.*
import org.scalajs.dom.HTMLHtmlElement
import org.scalajs.dom.TouchEvent
object Cursor:
  def draw(ev : MouseEvent,el : HTMLElement,content : HTMLElement) :Double=
    given MouseEvent = ev
    given HTMLElement = content
    val xVal = x.round.toInt
    draw(xVal,el)
    xVal
  def x(ev: TouchEvent)=
    val rect = ev.target.asInstanceOf[HTMLElement].getBoundingClientRect()
    val t  = if !scalajs.js.isUndefined( ev.targetTouches(0)) then  ev.targetTouches(0) else ev.changedTouches(0)
    (t.pageX - rect.left).toFloat.round
  def draw(ev : TouchEvent,el : HTMLElement,content : HTMLElement) :Double=
    
    given HTMLElement = content
    val xVal = x(ev).round.toInt
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
    content.events.mousedown[MouseEvent](_ => mousDown = true)
    moseHandleMove.events.mouseup[MouseEvent](_ => mousDown = false)
    moseHandleMove.events.mousemove[MouseEvent](e  => {
      if mousDown then 
        val value = draw(e,cursor,content)/100
        event(value )
        varValue.value = value
    })
    content.events.touchstart[TouchEvent](_ => mousDown = true)
    moseHandleMove.events.touchend[TouchEvent](_ => mousDown = false)
    moseHandleMove.events.touchmove[TouchEvent](e  => {
      if mousDown then 
        val value = draw(e,cursor,content)/100
        event(value )
        varValue.value = value
    })
    draw((ini*100).round.toInt,cursor)
    (varValue,content)
