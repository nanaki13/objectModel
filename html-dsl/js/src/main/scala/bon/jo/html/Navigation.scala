package bon.jo.html
import org.scalajs.dom.URLSearchParams
import org.scalajs.dom.window.location
import bon.jo.domain.UserContext
import bon.jo.common.typeutils.~
import bon.jo.domain.UserInfo.apply
import bon.jo.domain.UserInfo
object Navigation:
  case class Page(name : String)
  case class PageInfo(public : Boolean)
  case class DefaultPage(name : String)
  given URLSearchParams = URLSearchParams(location.search)
  type SiteMap = Map[Page,(PageInfo,UserContext) ?=> Unit]
  inline def params : ~[URLSearchParams] = summon
  inline def page : ~[Page]  = summon
  inline def defaultPage : ~[DefaultPage]  = summon
  given (using URLSearchParams,DefaultPage) : Page = Option(params.get("page")).map(Page(_)).getOrElse(Page(defaultPage.name))
  inline def siteMap : ~[SiteMap]  = summon

  object AnonymUser extends UserContext("",_ => UserInfo(-1,"Anonym",None))

  def navigate() : (SiteMap,Page,UserContext,PageInfo) ?=> Unit = 
    siteMap(page)
  def pages() : SiteMap ?=> Seq[Page] = 
    siteMap.keys.toSeq

