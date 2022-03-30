package bon.jo

import org.scalajs.dom.Element
import org.scalajs.dom.document
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.Text
import scala.language.dynamics
import org.scalajs.dom.Node
import org.scalajs.dom.MouseEvent
object MiniDsl extends scala.Dynamic:



  def apply(p : String):Element = document.createElement(p)
  def childs[T <: Element](e : List[Element]): T  ?=> T  = 
    e.foreach(summon[T].appendChild)
    summon
  def childs[T <: Element](e : Node *): T  ?=> T  = 
    e.foreach(summon[T].appendChild)
    summon
  def _class[T<: HTMLElement](e :String): T  ?=> T  = 
    summon.className = e
    summon
  def _text[T<: Element](s : String): T ?=> T = 
    summon.textContent = s
    summon
  def click[T<: HTMLElement](f : => Unit): T ?=> T = 
    summon.onclick = _ => f
    summon
  def click[T<: HTMLElement](f : MouseEvent => Unit): T ?=> T = 
    summon.onclick = e => f(e)
    summon
  def t(s : String):Text = document.createTextNode(s)
  
  def me[T <: Element](t : T=>Unit): T  ?=> T = 
    t(summon)
    summon

  def applyDynamic[T <: HTMLElement](p : String)(f : T ?=> T * ):T=
    given T = selectDynamic(p)
    f.map(ff => ff).last
  def selectDynamic[T <: Element](p : String):T = apply(p).asInstanceOf[T]