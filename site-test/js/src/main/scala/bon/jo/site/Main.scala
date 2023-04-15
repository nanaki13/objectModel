package bon.jo.site
import bon.jo.common.JsUtil.given
import bon.jo.common.FunBuilder.*
import model.*
import scalajs.js
import bon.jo.html.Html.PreDef.*
import bon.jo.html.Html.*
import org.scalajs.dom.document
import org.scalajs.dom.window
import org.scalajs.dom.console
import scala.concurrent.Future
import scala.collection.mutable
import concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.HTMLElement
import bon.jo.common.typeutils.~
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.HTMLButtonElement
object Main {

  inline def service: ~[PageContentService] = summon
  inline def target: Target ?=> HTMLElement = summon.value
  trait PageContentService:
    def get(p: Page): Future[PageContent]
    def update(p: PageContent): Future[PageContent]
  class StaticPageContent(data: mutable.Map[String, PageContent])
      extends PageContentService:
    def get(p: Page): Future[PageContent] = Future.successful(
      data.getOrElse(p.id, throw new RuntimeException("Page not found"))
    )
    def update(p: PageContent): Future[PageContent] =
      data.update(p.pageId, p)
      Future.successful(p)

  given PageContentService = StaticPageContent(
    mutable.Map(
      "Home" -> |[PageContent](
        $.content = "<h1>Super Page</h1>",
        $.pageId = "Home"
      ),
      "Blog" -> |[PageContent](
        $.content = "<h2>C'est super</h2>",
        $.pageId = "Blog"
      )
    )
  )
  val m: Menu = |(
    $.pages = js.Array(
      |($.title = "Home", $.id = "Home"),
      |($.title = "Blog", $.id = "Blog")
    )
  )
  case class Target(value: HTMLElement)
  @main
  def run(): Unit =

    val body = document.body
    given v: Target = Target(div())

    val menuHtml = (div(
      childs(
        m.pages
          .map(page => div(text(page.title)) --> a(href(""), click(go(page))))
          .toSeq*
      )
    ))
    menuHtml --> body
    v.value --> body
    val plusButton : Ref[HTMLButtonElement] = Ref()
    button(bind(plusButton),
      text("+"),
      click(_ => {
        *.disabled = true
        val txt = input("text")()
        txt --> menuHtml
        button(
          text("ok"),
          click(_ => {
            plusButton.value.disabled = false
            menuHtml.removeChild(*)
            menuHtml.removeChild(txt)
            val nPage = |[Page]($.title =txt.value, $.id = txt.value)
            menuHtml.insertBefore(div(text(nPage.title)) --> a(href(""), click(go(nPage))),plusButton.value)

            val nContent = |[PageContent](
              $.content = txt.value,
              $.pageId = txt.value
            )
            service.update(nContent)
          })
        ) --> menuHtml
      })
    ) --> menuHtml

  def go(p: Page)(e: MouseEvent): (PageContentService, Target) ?=> Unit =
    e.preventDefault()
    service.get(p).foreach { pc =>

      target.innerHTML = ""
      val nContent = div(*.innerHTML = pc.content)
      nContent --> target

      var editHtml = false
      var editRaw = false
      button(
        text("edit html"),
        click(_ => {
          editHtml = !editHtml
          nContent.contentEditable = editHtml.toString()
          if editHtml then
            *.textContent = "save"
            nContent.textContent = pc.content
          else
            *.textContent = "edit html"
            pc.content = nContent.innerText
            nContent.innerHTML = pc.content
            service.update(pc)
        })
      ) --> target
      button(
        text("edit"),
        click(_ => {
          editRaw = !editRaw
          nContent.contentEditable = editRaw.toString()
          if editRaw then *.textContent = "save"
          else
            *.textContent = "edit"
            pc.content = nContent.innerHTML
            service.update(pc)
        })
      ) --> target

    }

}
