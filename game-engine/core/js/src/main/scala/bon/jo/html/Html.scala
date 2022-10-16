package bon.jo.html

import org.scalajs.dom.{Element,document}
import scala.language.dynamics
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.CSSStyleDeclaration
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLAnchorElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLImageElement
object Html:
  object PreDef:
      extension (s : Any)
        def toDiv:HTMLElement = div(text(s.toString()))
        def toDiv(f : HTMLElement ?=> Unit *):HTMLElement = 
          given HTMLElement=  toDiv
          f.foreach(ff => ff)
          summon
      extension (s : HTMLElement)
        def wrapDiv:HTMLElement = div(childs(s))
        def wrapDiv(f : HTMLElement ?=> Unit):HTMLElement = 
          given HTMLElement=  wrapDiv
          f
          summon
      extension (s : Seq[HTMLElement])
        def wrapDiv:HTMLElement = div(childs(s *))
        def wrapDiv(f : HTMLElement ?=> Unit):HTMLElement = 
          given HTMLElement=  wrapDiv
          f
          summon
      def div(f: HTMLElement ?=> Unit*): HTMLElement = <.div[HTMLElement](f*)
      def a(f: HTMLAnchorElement ?=> Unit*): HTMLAnchorElement = <.a[HTMLAnchorElement](f*)
      def href(href : String): HTMLAnchorElement ?=> Unit = summon.href = href
      def input(tpe : String)(f: HTMLInputElement ?=> Unit*): HTMLInputElement = 
        val ret = <.input[HTMLInputElement](f*)
        ret.`type` = tpe
        ret
      def button(f: HTMLButtonElement ?=> Unit*): HTMLButtonElement =
        <.button[HTMLButtonElement](f*)
      def div: HTMLElement = <.div[HTMLElement]
      def image: HTMLImageElement = <.img[HTMLImageElement]
      def scrollMax =
        document.documentElement.scrollHeight - document.documentElement.clientHeight
  case class Ref[T](var value : T = null)
  extension [T <:HTMLElement] (e : Ref[T])
    inline def bindMe() : T ?=> Unit = bind(e)
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
  def _class[T <: HTMLElement](c : String):T ?=> Unit = 
    summon.className = c
  def bind[T <: HTMLElement](ref : Ref[T]):T ?=> Unit = 
    ref.value = summon
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
