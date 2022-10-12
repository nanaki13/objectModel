package bon.jo.pong
import bon.jo.pong.Login.UserContext
import concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.document
import scalajs.js.Date
import bon.jo.service.PostService
import bon.jo.service.SaveResult
import bon.jo.domain.*
import bon.jo.domain.fake
import bon.jo.html.Navigation.*
import bon.jo.html.Navigation.given
import scala.concurrent.Future
import bon.jo.pong.service.PostServiceRest
import bon.jo.html.Html.*
object Main:
  @main
  def launch(): Unit =

    var posts = fake.PostSubject(15,"Forum",()=>  (Date()).toString )
    /*given PostService with
      def readPosts(subjectId : Int,from : Int,size : Int):Future[Seq[PostUser]] = 
        println((from : Int,size : Int))
        Future.successful( posts.posts.slice(from,from+size))
      def addPost(subjectId : Int,userId : Long,content : String):Future[SaveResult] = Future.successful(SaveResult.OK(Post(subjectId,userId,new Date().toDateString(),content)))*/
      
    given SiteMap = Map(Page("game")-> PongGamePage.go,Page("forum")->{
      given PostService = PostServiceRest()
      ForumPage.go(posts.postSubjectTitle.title,0,10)
    })
    document.body :++ (MenuPage.menu(),Login.logoutButton())
    given DefaultPage = DefaultPage("game")
    Login.log().foreach(f => 
        given UserContext = f
        navigate()
      )
