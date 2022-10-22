package bon.jo.pong
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import bon.jo.html.Navigation.*
import org.scalajs.dom.HTMLAnchorElement
import org.scalajs.dom.HTMLElement
object MenuPage:

  def pagesLink(): SiteMap ?=> Seq[HTMLAnchorElement] =
    siteMap.keys.toSeq.map { page =>
      a(text(page.name), href(s"?page=${page.name}"))
    }
  def menu(): SiteMap ?=> HTMLElement =
    div(childs(pagesLink().map(e => div(childs(e))) *), _class("menu"))
