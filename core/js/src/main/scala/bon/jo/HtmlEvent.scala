package bon.jo

import org.scalajs.dom.Element
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.Event
import scala.language.dynamics
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.document
object HtmlEvent {
  def html(using  HTMLElement):HTMLElement = summon
  object EventAdder extends scala.Dynamic:
    def applyDynamic[E <: Event](eventName : String)(consumer  : E => Unit)(using HTMLElement) = html.addEventListener[E](eventName,consumer)
  trait EventAdder(using HTMLElement) extends scala.Dynamic:
    def applyDynamic[E <: Event](eventName : String)(consumer  : E => Unit) = html.addEventListener[E](eventName,consumer)


  extension ( e : HTMLElement)
    def events : EventAdder = 
      given HTMLElement = e
      new {}

    def onmousedownandmove(ee : MouseEvent => Unit):Unit = 
      
      var mouseDown = false
      {
        given HTMLElement = e
        EventAdder.mousedown{_ => mouseDown=true}
        
      }
      {
        given HTMLElement = document.body
        EventAdder.mousemove[MouseEvent]{ev => if mouseDown then ee(ev)}
        EventAdder.mouseup{_ => mouseDown=false}
      }
}
