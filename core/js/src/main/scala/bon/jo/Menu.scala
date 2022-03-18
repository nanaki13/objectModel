package bon.jo
import MiniDsl as !
import !.*
import org.scalajs.dom.HTMLAnchorElement
import bon.jo.Menu.MenuItem
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.HTMLUListElement
import org.scalajs.dom.HTMLLIElement
import org.scalajs.dom.HTMLElement

class  Menu[T <: MenuItem](val childsp : List[T],event : T => HTMLElement,out : HTMLElement) :
  val root = !.div[HTMLDivElement](_class("menu"))
  val childsHtml =  childsp
  .map(e => 
    val link = !.a[HTMLAnchorElement](_text(e.text),_class("menu-link"),me(_.href=""))
    link.onclick = ev => 
      ev.preventDefault()
      out.innerHTML = ""
      out.append(event(e))
    link
    )
  .map(a => !.li[HTMLLIElement](childs(a)))
  val bagChilds = !.ul[HTMLUListElement](childs(childsHtml))
  root.append(bagChilds)
object Menu:
  trait MenuItem:
    def text : String

