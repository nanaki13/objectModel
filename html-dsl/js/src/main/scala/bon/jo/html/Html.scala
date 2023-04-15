package bon.jo.html

import org.scalajs.dom.{Element,document}
import scala.language.dynamics
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.CSSStyleDeclaration
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLAnchorElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLImageElement
import org.scalajs.dom.MouseEvent
import scala.annotation.targetName

object Html:
  val < = Create 
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
      def click[T <: HTMLElement](f: MouseEvent => Unit): HTMLElement ?=> Unit =
        summon.addEventListener("click",f)
      def div: HTMLElement = <.div[HTMLElement]
      def image: HTMLImageElement = <.img[HTMLImageElement]
      def image(f: HTMLImageElement ?=> Unit*): HTMLImageElement = <.img[HTMLImageElement](f*)
      def src(srcP : String): HTMLImageElement ?=> Unit = 
        summon.src = srcP
      def scrollMax =
        document.documentElement.scrollHeight - document.documentElement.clientHeight
  case class Ref[T](private [html] var _value : Option[T] = None):
    def value : T = _value.get
  extension [T <:HTMLElement] (e : Ref[T])
    inline def bindMe() : T ?=> Unit = bind(e)
  extension [E <: Element](e : E)
    def clear():Unit = e.children.foreach(_.remove())
   
    def `>`(ee : E => Unit *):E = 
      ee.foreach(_(e))
      e
    def >>(ee : E ?=> Unit *):E = 
      given E = e
      ee.foreach(f => f)
      e
    def :+(ee : Element):E = 
      e.appendChild(ee)
      e
    def :++(ee : Element *):E = 
      e.append(ee *)
      e

    def -->[T <: Element](o : T):T = 
      o.append(e)
      o
  def _class[T <: HTMLElement](c : String):T ?=> Unit = 
    summon.className = c
  def bind[T <: HTMLElement](ref : Ref[T]):T ?=> Unit = 
    ref._value = Some(summon)
  def text[T <: Element](tag : String):T ?=> Unit = 
    summon.textContent = tag
  def style[T <: HTMLElement](ee : CSSStyleDeclaration => Unit *):T ?=> Unit = 
    ee.foreach(_(summon.style))
  def childs[T <: Element](ee : Element *):T ?=> Unit = 
    summon.:++(ee *)
  inline def *[T <: Element]:T ?=>T = summon
