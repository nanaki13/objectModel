package bon.jo
import MiniDsl as !
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.HTMLSpanElement
import org.scalajs.dom.HTMLUListElement
import org.scalajs.dom.HTMLLIElement
import org.scalajs.dom.HTMLAnchorElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLTextAreaElement
import org.scalajs.dom.HTMLPreElement
import org.scalajs.dom.HTMLSelectElement
import org.scalajs.dom.HTMLOptionElement
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.Node
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLLabelElement

object HtmlPredef {
  inline def div(f : OnHtmlSelf[HTMLDivElement] *): HTMLDivElement = !.div(f  *)
  inline def span(f : HTMLSpanElement ?=> HTMLSpanElement *): HTMLSpanElement = !.span(f  *)
  inline def ul(f : HTMLUListElement ?=> HTMLUListElement *): HTMLUListElement = !.ul(f  *)
  inline def li(f : HTMLLIElement ?=> HTMLLIElement *): HTMLLIElement = !.li(f  *)
  inline def a(f : HTMLAnchorElement ?=> HTMLAnchorElement *): HTMLAnchorElement = !.a(f  *)
  inline def button(f : HTMLButtonElement ?=> HTMLButtonElement *): HTMLButtonElement = !.button(f  *)
  inline def input(f : HTMLInputElement ?=> HTMLInputElement *): HTMLInputElement = !.input(f  *)
  inline def inputCheck(check : Boolean  = false) (f : HTMLInputElement ?=> HTMLInputElement *): HTMLInputElement = input(!.me(_.`type`="checkbox"),!.me(_.checked=check))
  inline def label(f : HTMLLabelElement ?=> HTMLLabelElement *): HTMLLabelElement = !.label(f  *)
  inline def textarea(f : HTMLTextAreaElement ?=> HTMLTextAreaElement *): HTMLTextAreaElement = !.textarea(f  *)
  inline def pre(f : HTMLPreElement ?=> HTMLPreElement *): HTMLPreElement = !.pre(f  *)
  inline def select_(f : HTMLSelectElement ?=> HTMLSelectElement *): HTMLSelectElement = !.select(f * )
  inline def option(f : HTMLOptionElement ?=> HTMLOptionElement *): HTMLOptionElement = !.option(f  *)
  inline def canvas(f : HTMLCanvasElement ?=> HTMLCanvasElement *): HTMLCanvasElement = !.canvas(f  *)


  inline def div: HTMLDivElement = !.div
  inline def span: HTMLSpanElement = !.span
  inline def ul: HTMLUListElement = !.ul
  inline def li: HTMLLIElement = !.li
  inline def a: HTMLAnchorElement = !.a
  inline def button: HTMLButtonElement = !.button
  inline def input: HTMLInputElement = !.input
  inline def inputCheck(check : Boolean ): HTMLInputElement = input(!.me(_.`type`="checkbox"),!.me(_.checked=check))
  inline def label: HTMLLabelElement = !.label
  inline def textarea: HTMLTextAreaElement = !.textarea
  inline def pre: HTMLPreElement = !.pre
  inline def select: HTMLSelectElement = !.select
  inline def option: HTMLOptionElement = !.option
  inline def canvas:HTMLCanvasElement = !.canvas

  type OnHtml[A <: HTMLElement,B] = A ?=> B
  type OnHtmlU[A <: HTMLElement] = OnHtml[A,Unit]
  type OnHtmlSelf[A <: HTMLElement] = OnHtml[A,A]
  type OnHTMLElement = OnHtmlU[HTMLElement]
  object OnHtml:
    def apply[A <: HTMLElement]():OnHtmlSelf[A] = summon

}
