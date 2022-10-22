package bon.jo.pong
import bon.jo.pong.Login.UserContext
import bon.jo.pong.HttpServiceConfig.AuthParam.given
import bon.jo.pong.AvatarView
import concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.document
import scalajs.js.Date
import bon.jo.service.PostService
import bon.jo.service.SaveResult
import bon.jo.domain.*
import bon.jo.domain.fake
import bon.jo.common.Strings.FileExtension
import bon.jo.html.Navigation.*
import bon.jo.html.Navigation.given
import scala.concurrent.Future
import bon.jo.pong.service.PostServiceRest
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.RequestInit
import org.scalajs.dom.FormData
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.console
import org.scalajs.dom.URL
import org.scalajs.dom.HTMLElement
import bon.jo.common.SideEffect.Serveur
object avatarServeur extends Serveur[String]
object Main:
  @main
  def launch(): Unit =


   
    given Serveur[String] = avatarServeur
    given SiteMap = 
      Map[Page,UserContext ?=> Unit](Page("game")-> {
        PongGamePage.go},
        
        Page("forum")->{
        given PostService = PostServiceRest()
        ForumPage.go("Forum",0,10)
      })
      
    

    document.body :+ MenuPage.menu()
    given DefaultPage = DefaultPage("game")
    Login.log().foreach(f => 
        given UserContext = f
        val avatarViewRef = Ref[HTMLElement]()
        document.body :+ div(_class("top-right d-flex"), childs(div(childs(AvatarView.view(avatarViewRef)),bind(avatarViewRef)),Login.logoutButton()))
        navigate()
      )
