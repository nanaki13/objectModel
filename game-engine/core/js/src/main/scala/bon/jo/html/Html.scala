package bon.jo.html

import org.scalajs.dom.{Element,document}
import scala.language.dynamics
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.CSSStyleDeclaration
object Html:
  extension [E <: Element](e : E)
    def >(ee : E => Unit *):E = 
      ee.foreach(_(e))
      e
    def :+(ee : Element):E = 
      e.appendChild(ee)
      e
    def :++(ee : Element *):E = 
      e.append(ee *)
      e
  def text[T <: Element](tag : String):T ?=> Unit = 
    summon.textContent = tag
  def style[T <: HTMLElement](ee : CSSStyleDeclaration => Unit *):T ?=> Unit = 
    ee.foreach(_(summon.style))
  def childs[T <: Element](ee : Element *):T ?=> Unit = 
    summon.:++(ee *)
  object < extends scala.Dynamic:

    def applyDynamic[T <: Element](tag : String)(f : T ?=> Unit *) : T = 
      given t : T = document.createElement(tag).asInstanceOf
      f.foreach(ff => ff)
      t
    def selectDynamic[T <: Element](tag : String) : T = 
      document.createElement(tag).asInstanceOf[T]
