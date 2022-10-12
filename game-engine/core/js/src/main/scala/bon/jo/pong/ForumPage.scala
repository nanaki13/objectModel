package bon.jo.pong

import bon.jo.html.Html.*

import bon.jo.service.SaveResult

import bon.jo.service.SaveResultEx
import bon.jo.service.SaveResultSuccess
import bon.jo.service.PostService
import bon.jo.service.PostService.postService
import bon.jo.request.BadStatusException
import bon.jo.domain.Post
import bon.jo.domain.PostInfo
import bon.jo.common.typeutils.~
import org.scalajs.dom.document
import org.scalajs.dom.window
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLCanvasElement
import scalajs.js.special.debugger
import org.scalajs.dom.console
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import org.scalajs.dom.HTMLButtonElement
object ForumPage:
  def div(f: HTMLElement ?=> Unit*): HTMLElement = <.div[HTMLElement](f*)
  def button(f: HTMLButtonElement ?=> Unit*): HTMLButtonElement =
    <.button[HTMLButtonElement](f*)
  def div: HTMLElement = <.div[HTMLElement]
  def scrollMax =
    document.documentElement.scrollHeight - document.documentElement.clientHeight
  def addPost()(using Login.UserContext, PostService,Component): HTMLElement =
    val buttonRef: Ref[HTMLButtonElement] = Ref()
    val contentPost: Ref[HTMLElement] = Ref()
    val newPostCont = div(
      _class("posts"),
      childs(
        div {
          _class("post")
          childs(
            div(
              text(s"${Login.UserContext().user.name} : "),
              _class("post-user")
            ),
            div(bind(contentPost), _class("post-content"))
              .>(_.contentEditable = "true"),
            div(childs(button(text("send"), bind(buttonRef))))
          )
        }
      )
    )
    buttonRef.value.onclick = e => 
      postService.addPost(1,PostInfo(contentPost.value.innerHTML)).foreach{
        postOK => 
          target :+ postHtml(Login.UserContext().user.name,postOK.postDateTime,postOK.content)
          contentPost.value.textContent=""
          window.scrollTo(
            0,
            scrollMax
          )
     
      }
        
        

    newPostCont
  def go(subjectTitle : String,from: Int, size: Int)(using Login.UserContext, PostService): Unit =
    val posts = div(_class("posts"))

    given Component = Component( div(text(subjectTitle), _class("post-title")),posts)
    document.body :+ target
    document.body :+ addPost()
    goOn(from, size)
   
    var count = 1
    document.addEventListener(
      "scroll",
      e => if (window.scrollY == 0) then 
        goOn(from + size*count, size)
        count +=1
    )
  def postHtml(user:  String, date : String, content : String) = 
    div {
          _class("post")
          childs(
            div( childs(div(text(s"$user"),_class("post-user")),div(text(s"$date"), _class("post-date")))),
            
            div(_class("post-content")).>(_.innerHTML=content)
          )
        }
  inline def target: Component ?=> HTMLElement = compnent.target
  case class Component(title : HTMLElement,target : HTMLElement)
  inline def compnent: ~[Component] = summon
  def goOn(from: Int, size: Int)( using
      Login.UserContext,
      PostService,Component
  ): Unit =
    postService.readPosts(1, from, size).foreach { e =>
      val postHtmlEl = e.map { p=> postHtml(p.user.name,p.postDateTime,p.content)}.reverse
      

      if target.childNodes.length == 0 then
       // target :+ compnent.title
        target.:++(postHtmlEl*)
        window.scrollTo(
          0,
          scrollMax
        )
        
      else
        val deb = target.children(1).asInstanceOf[HTMLElement]


        postHtmlEl.foreach(p => target.insertBefore(p, deb))

        window.scrollTo(
          0,
          deb
            .getBoundingClientRect()
            .top
            .toInt - target.children(0).asInstanceOf[HTMLElement].clientHeight
        )

    }
