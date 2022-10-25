package bon.jo.pong

import bon.jo.html.Html.*

import bon.jo.service.SaveResult

import bon.jo.service.SaveResultEx
import bon.jo.service.SaveResultSuccess
import bon.jo.service.PostService
import bon.jo.service.PostService.postService
import bon.jo.html.request.BadStatusException
import bon.jo.domain.Post
import bon.jo.domain.PostInfo
import bon.jo.common.typeutils.~
import org.scalajs.dom.document
import org.scalajs.dom.window
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLCanvasElement
import scalajs.js.special.debugger
import scalajs.js
import org.scalajs.dom.console
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.Event
import bon.jo.domain.UserInfo
import bon.jo.html.request.HttpRequest.GlobalParam
import bon.jo.domain.UserContext
import bon.jo.common.SideEffect.Serveur
object ForumPage:
  def div(f: HTMLElement ?=> Unit*): HTMLElement = <.div[HTMLElement](f*)
  def button(f: HTMLButtonElement ?=> Unit*): HTMLButtonElement =
    <.button[HTMLButtonElement](f*)
  def div: HTMLElement = <.div[HTMLElement]
  def scrollMax =
    document.documentElement.scrollHeight - document.documentElement.clientHeight
  def addPost()(using UserContext, PostService, Component,GlobalParam,Serveur[String]): HTMLElement =
    val buttonRef: Ref[HTMLButtonElement] = Ref()
    val contentPost: Ref[HTMLElement] = Ref()
    val newPostCont = div(
      _class("posts"),
      childs(
        div {
          _class("post")
          childs(
            div(
              text(s"${UserContext().user.name} : "),
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
      postService.addPost(1, PostInfo(contentPost.value.innerHTML)).foreach {
        postOK =>
          target :+ postHtml(
            UserContext().user,
            postOK.postDateTime,
            postOK.content
          )
          contentPost.value.textContent = ""
          window.scrollTo(
            0,
            scrollMax
          )

      }

    newPostCont
  trait CountListne:
    var count = 1
    val listener: js.Function1[Event, ?]
  def go(subjectTitle: String, from: Int, size: Int)(using
      UserContext,
      PostService,GlobalParam,Serveur[String]
  ): Unit =
    val posts = div(_class("posts"))

    given Component =
      Component(div(text(subjectTitle), _class("post-title")), posts)
    document.body :+ target
    document.body :+ addPost()

    lazy val cntLiten: CountListne = new CountListne:
      val listener: js.Function1[Event, ?] =
        e =>
          if (window.scrollY == 0) then
            goOn(from + size * count, size, cntLiten)
            count += 1
    goOn(from, size, cntLiten)

  def postHtml(user: UserInfo, date: String, content: String) : (GlobalParam,UserContext,Serveur[String])?=> HTMLElement =
    div {
      _class("post")
      childs(
        div(
          childs(
            AvatarView.view(user),
            div(text(s"${user.name}"), _class("post-user")),
            div(text(s"$date"), _class("post-date"))
          )
        ),
        div(_class("post-content")).>(_.innerHTML = content)
      )
    }
  inline def target: Component ?=> HTMLElement = compnent.target
  case class Component(title: HTMLElement, target: HTMLElement)
  inline def compnent: ~[Component] = summon
  def goOn(from: Int, size: Int, listener: CountListne)(using
      UserContext,
      PostService,
      Component,GlobalParam,Serveur[String]
  ): Unit =
    postService.readPosts(1, from, size).foreach { e =>
      if e.nonEmpty then 
        document.addEventListener("scroll", listener.listener)
        val postHtmlEl = e.map { p =>
          postHtml(p.user, p.postDateTime, p.content)
        }.reverse
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
      else 
        document.removeEventListener( "scroll",listener.listener)


    }
