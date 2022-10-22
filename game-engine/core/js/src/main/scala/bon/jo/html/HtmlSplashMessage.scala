package bon.jo.html

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document
import bon.jo.html.Html.Ref
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*

trait HtmlSplashMessage:
  val root : HTMLElement
  val dialog: HTMLElement
  

  def addText(strs : String *):Unit = dialog.:++ (strs.map(str =>div(text(str))) *)
  def show():Unit =  
    document.body.appendChild(root)
  def isShow():Boolean = document.body.contains(root)


object HtmlSplashMessage:
  class Impl(ok: => Unit,confirmText : String) extends HtmlSplashMessage:
      val dialogRef : Ref[HTMLElement] = Ref()
      lazy val cont: HTMLElement = <.div[HTMLElement] {
        _class("splash")
        childs(
          <.div[HTMLElement] {
            _class("flex-center")
            childs(
              <.div[HTMLElement] {
                _class("dialog")
                childs(
                  <.div[HTMLElement](bind(dialogRef)),
                  <.div[HTMLElement] {
                    childs(
                      <.button[HTMLElement](text(confirmText)).>(
                        _.onclick = _ => {
                          ok
                          document.body.removeChild(cont)
                        }
                      )
                    )
                  }
                )
              }
            )
          }
        )
      }
      val root : HTMLElement = cont
      val dialog: HTMLElement =dialogRef.value
  def apply(text : String,ok: => Unit,confirmText : String ):HtmlSplashMessage = 
    val ret = apply(ok,confirmText)
    ret addText text
    ret
  def apply(ok: => Unit,confirmText : String): HtmlSplashMessage = Impl(ok,confirmText)
   
