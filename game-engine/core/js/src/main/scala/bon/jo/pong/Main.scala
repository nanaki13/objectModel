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
object Main:
  @main
  def launch(): Unit =

    var posts =
       div.style
       fake.PostSubject(15,"Forum",()=>  (Date()).toString )
    /*given PostService with
      def readPosts(subjectId : Int,from : Int,size : Int):Future[Seq[PostUser]] = 
        println((from : Int,size : Int))
        Future.successful( posts.posts.slice(from,from+size))
      def addPost(subjectId : Int,userId : Long,content : String):Future[SaveResult] = Future.successful(SaveResult.OK(Post(subjectId,userId,new Date().toDateString(),content)))*/
    def testinput : UserContext ?=> Unit =
      val inuptRef = Ref[HTMLInputElement]()
      val buttonRef = Ref[HTMLButtonElement]() 
      div(childs(
        div(childs( input("file")(inuptRef.bindMe()))),
        div(childs( button(text("send"),buttonRef.bindMe())))
      ),style(_.marginLeft="5em")).>(document.body.appendChild)
  
      inuptRef.value.onchange =event => 
         if(inuptRef.value.files.length > 0) then{
            val src = URL.createObjectURL(inuptRef.value.files(0));
            val preview = image
            preview.src = src;
            preview.style.display = "block";
            document.body.appendChild(preview)
         }
      buttonRef.value.onclick = e => 
        val formData = new FormData();
        val name = inuptRef.value.files(0).name
        name match
          case FileExtension(_,ext) => formData.append("ext",ext)
        

        formData.append("image", inuptRef.value.files(0))
        Login.userServicePrivate.sendAvatar(formData)
    

      

    given SiteMap = Map(Page("game")-> PongGamePage.go,Page("forum")->{
      given PostService = PostServiceRest()
      ForumPage.go(posts.postSubjectTitle.title,0,10)
    },Page("test")-> testinput)
    document.body :+ MenuPage.menu()
    given DefaultPage = DefaultPage("game")
    Login.log().foreach(f => 
        given UserContext = f
        document.body :+ div(_class("top-right"), childs(AvatarView.view,Login.logoutButton()))
        navigate()
      )
