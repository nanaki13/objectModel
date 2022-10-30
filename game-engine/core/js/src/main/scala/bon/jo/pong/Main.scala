package bon.jo.pong
import bon.jo.domain.UserContext
import bon.jo.html.HttpServiceConfig.AuthParam.given
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
import org.scalajs.dom.window
import org.scalajs.dom.Screen
import org.scalajs.dom.URL
import org.scalajs.dom.HTMLElement
import bon.jo.common.SideEffect.Serveur
import bon.jo.service.ScoreService
import bon.jo.pong.service.ScoreServiceRest
import scala.util.Failure
import scala.util.Success
import bon.jo.pong.SysBuilder.SysParam
object avatarServeur extends Serveur[String]
object Main:
  val gamePage = Page("Game")
  val forumPage = Page("Forum")
  val editPage =  Page("EditLvl")
  @main
  def launch(): Unit =
    given Serveur[String] = avatarServeur
    val scorePage : Option[(Page,(PageInfo,UserContext) ?=> Unit)]  = 
      window.screen.asInstanceOf[scalajs.js.Dynamic].orientation.`type`.asInstanceOf[String].head match
        case 'p' => Some(Page("Score") -> {
          given ScoreService = ScoreServiceRest()
          TopScoreView.view(1).onComplete{
            
              case Failure(exception) => 
              case Success(value) => document.body :+ div(_class("root"),childs(div(_class("dialog flex-center"),childs(value)))) 
              
            }
        })
        case _ => None
    
   

    given pageInfo : PageInfo = 
      page match
        case Main.gamePage => PageInfo(false)
        case Main.forumPage => PageInfo(false)
        case _ =>  PageInfo(true)
      
    given SiteMap = 
      val ret = Map[Page,(PageInfo,UserContext) ?=> Unit](gamePage-> {
        given ScoreService = ScoreServiceRest()
        PongGamePage.go},
        editPage-> {
        given ScoreService = ScoreServiceRest()
        
        (new PongGamePage(PongGamePage.pngSystem) with EditLevelView).editLvl()},
        
        forumPage->{
        given PostService = PostServiceRest()
        ForumPage.go("Forum",0,10)
      })
      scorePage match
        case None => ret
        case Some(value) => ret + value
      
      
    

    document.body :+ MenuPage.menu()
    given DefaultPage = DefaultPage("Game")

    
    pageInfo match
      case PageInfo(true) => 
        given UserContext = AnonymUser
        navigate()
      case _ =>  
        Login.log().foreach(f => 
            given UserContext = f
            val avatarViewRef = Ref[HTMLElement]()
            document.body :+ div(_class("top-right d-flex"), childs(div(childs(AvatarView.view(avatarViewRef)),bind(avatarViewRef)),Login.logoutButton()))
            navigate()
          )
