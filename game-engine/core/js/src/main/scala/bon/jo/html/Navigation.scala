package bon.jo.html
import org.scalajs.dom.URLSearchParams
import org.scalajs.dom.window.location
import bon.jo.pong.Login.UserContext
import bon.jo.common.typeutils.~
object Navigation:
  println("Navigation : ")
  case class Page(name : String):
    println("page : "+name)
  case class DefaultPage(name : String)
  given URLSearchParams = URLSearchParams(location.search)
  type SiteMap = Map[Page,UserContext ?=> Unit]
  inline def params : ~[URLSearchParams] = summon
  inline def page : ~[Page]  = summon
  inline def defaultPage : ~[DefaultPage]  = summon
  given (using URLSearchParams,DefaultPage) : Page = Option(params.get("page")).map(Page.apply).getOrElse(Page(defaultPage.name))
  inline def siteMap : ~[SiteMap]  = summon


  def navigate() : (SiteMap,Page,UserContext) ?=> Unit = 
    siteMap(page)
  def pages() : SiteMap ?=> Seq[Page] = 
    siteMap.keys.toSeq

