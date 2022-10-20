package bon.jo.pong
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import org.scalajs.dom.HTMLElement
import bon.jo.pong.Login.UserContext
import bon.jo.request.HttpRequest.GlobalParam
import bon.jo.domain.UserInfo
object AvatarView {
  def view(using  UserContext, GlobalParam):HTMLElement =
    println(UserContext.user)
    UserContext.user.avatar match
      case None =>  div(text("+")) //DefineAvatarView()
      case Some(_) => view(Login.myAvatarUrl)
    
   // view(Login.myAvatarUrl)
  def view(u : UserInfo)(using GlobalParam):HTMLElement =
    view(Login.avatartUrl(u))
  inline def view(srcp : String):HTMLElement = <.div[HTMLElement](_class("avatar-wrapper"),childs(image(_class("avatar-img"),src(srcp))))
}
