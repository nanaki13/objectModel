package bon.jo.pong
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import org.scalajs.dom.HTMLElement
import bon.jo.domain.UserContext
import bon.jo.html.request.HttpRequest.GlobalParam
import bon.jo.domain.UserInfo
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.document
import org.scalajs.dom.URL
import org.scalajs.dom.FormData
import bon.jo.common.Strings.FileExtension
import bon.jo.html.HtmlSplashMessage.apply
import bon.jo.html.HtmlSplashMessage
import bon.jo.pong.Login.given
import concurrent.ExecutionContext.Implicits.global
import bon.jo.common.SideEffect
import scala.util.Try
import bon.jo.common.SideEffect.Serveur
import bon.jo.common.SideEffect.serveur
import org.scalajs.dom.HTMLImageElement
object AvatarView {

  type AvatarSplashUpdate = HtmlSplashMessage with CustomizeHtmlSplashMessage
  class CustomizeHtmlSplashMessage(avatarViewRef : Ref[HTMLElement])(ok: => Unit,confirmText : String)(using  UserContext, GlobalParam,Serveur[String]) extends HtmlSplashMessage.Impl(ok,confirmText):
 
  //    var _init = false
    //  def isInit(): Boolean = _init
   //   def init():Unit = 
      val inuptRef = Ref[HTMLInputElement]()
      val buttonRefTarget = Ref[HTMLElement]() 
      val preveiewTarget = Ref[HTMLElement]() 
      val imgTarget = Ref[HTMLElement]() 
      val buttonSend = button(text("send"))
      var url : String = ""
      given AvatarSplashUpdate = this
      dialog :+
      div(childs(
        div(text("Choose your Avatar! (max 1Mo)")),
        div(childs( input("file")(inuptRef.bindMe()).>(_.accept="""image/*"""))),
        div(buttonRefTarget.bindMe()),
        div(imgTarget.bindMe())
      ),style(_.marginLeft="5em"))
    

      inuptRef.value.onchange =event => 
          if(inuptRef.value.files.length > 0) then{
            val f = inuptRef.value.files(0)
            if f.size <= 1024 * 1024 then
              buttonRefTarget.value.appendChild(buttonSend)
              url = URL.createObjectURL(inuptRef.value.files(0))
              val preview = view(url) >> bind(preveiewTarget)
              imgTarget.value.clear()
              imgTarget.value.appendChild(preview)
            else
              Try(buttonRefTarget.value.removeChild(buttonSend))
              HtmlSplashMessage("File is too big!",(),"OK").show()
          }
      buttonSend.onclick = e => 
        val formData = new FormData();
        val name = inuptRef.value.files(0).name
        name match
          case FileExtension(_,ext) => formData.append("ext",ext)
        

        val service = Login.userServicePrivate
        formData.append("image", inuptRef.value.files(0))
        service.sendAvatar(formData).map(_.okWith[String,String](200)).map{
          r => 
            service.me().foreach{
              ui => 
                serveur.emit(url)
                avatarViewRef.value.clear()
                avatarViewRef.value :+ (preveiewTarget.value >> click(_ => avatarSendView(avatarViewRef)))
            }
            
        }
      // _init = true
    //  end init

  
  def view(avatarViewRef : Ref[HTMLElement])(using  UserContext, GlobalParam,Serveur[String]):HTMLElement =
    given AvatarSplashUpdate=  
      new CustomizeHtmlSplashMessage(avatarViewRef)((),"Close")
    UserContext.user.avatar match
      case None =>  div(childs(button(text("+"),click{
       _ => avatarSendView(avatarViewRef)
      }))) //DefineAvatarView()
      case Some(_) =>  (view(Login.myAvatarUrl)).wrapDiv( click(_ => avatarSendView(avatarViewRef)))
    
   // view(Login.myAvatarUrl)
  def view(u : UserInfo,avatarViewRef : Ref[HTMLElement])(using  AvatarSplashUpdate,UserContext, GlobalParam):HTMLElement =
    view(Login.avatartUrl(u)) >> click(_ => avatarSendView(avatarViewRef))
  def view(u : UserInfo)(using GlobalParam,UserContext,Serveur[String]):HTMLElement =

    val (v,img ) = viewAndImg(Login.avatartUrl(u)) 
    if u.id == UserContext().user.id then
      SideEffect.serveur.register{
        nSrc => 
          img.src = nSrc
      }
    v
  inline def view(srcp : String):HTMLElement = <.div[HTMLElement](_class("avatar-wrapper"),childs(image(_class("avatar-img"),src(srcp))))
  inline def viewAndImg(srcp : String):(HTMLElement, HTMLImageElement) =
    val refImd = Ref[HTMLImageElement]()
    val div =  <.div[HTMLElement](_class("avatar-wrapper"),childs(image(_class("avatar-img"),src(srcp),bind(refImd))))
    (div,refImd.value)

  def avatarSendView(avatarViewRef : Ref[HTMLElement])(using  ui :AvatarSplashUpdate) : (  UserContext, GlobalParam) ?=> Unit =
    if !ui.isShow() then ui.show()
}
