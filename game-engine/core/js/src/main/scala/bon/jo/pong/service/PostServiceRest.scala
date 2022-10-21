package bon.jo.pong.service

import bon.jo.pong.Login.UserContext
import bon.jo.service.PostService
import bon.jo.request.HttpRequest
import bon.jo.request.HttpRequest.Method.{POST, GET}
import bon.jo.pong.HttpServiceConfig.AuthParam.given
import scala.concurrent.Future
import bon.jo.service.SaveResult
import bon.jo.pong.Login.given
import bon.jo.pong.Login.UserInfoJs
import concurrent.ExecutionContext.Implicits.global
import scalajs.js
import bon.jo.pong.Login
import bon.jo.domain.UserInfo
import bon.jo.request.HttpRequest.given
import bon.jo.domain.PostUser
import bon.jo.domain.PostInfo
import bon.jo.domain.Post

object PostServiceRest {
    //  case class PostInfo(content: String)
    // case class PostUser(idSubject: Int,user: UserInfo, postDateTime: String, content: String)
    extension [T](t : T)
      def toSome:Some[T] = Some(t)
    trait PostInfoJs extends js.Object:
      val content: String
     trait PostJs extends PostInfoJs:
      val idSubject: Int
      val idUser: Double
      val postDateTime: String
    trait PostUserJs extends PostInfoJs:
      val idSubject: Int
      val user : UserInfoJs
      val postDateTime: String
    object PostUserJsParser:
      def unapply(u : PostUserJs) : Some[PostUser] = 
        PostUser(u.idSubject, Login.userInfo(u.user),u.postDateTime,u.content).toSome
    object PostJsParser:
      def unapply(u : PostJs) : Some[Post] = 
        Post(u.idSubject, u.idUser.toLong,u.postDateTime,u.content).toSome
    object PostInfoJs:
      def apply(postInfo : PostInfo): PostInfoJs =
        js.Dynamic.literal(content = postInfo.content).asInstanceOf
    given Conversion[js.Any,PostUser] = 
      e => 
        val PostUserJsParser(value) = e.asInstanceOf[PostUserJs]
        value
    given Conversion[js.Any,Post] = 
      e => 
        val PostJsParser(value) = e.asInstanceOf[PostJs]
        value
    given Conversion[String,String] = e=>e 
    class PostServiceImpl()(using UserContext) extends PostService with HttpRequest.Service :
      override def readPosts(subjectId: Int, from: Int, size: Int): Future[Seq[PostUser]] = 
        GET.sendOn(s"/$subjectId/posts?from=$from&size=$size").map(_.okWithJs[Seq[PostUser],String](200)) 

      override def addPost(subjectId: Int, content: PostInfo): Future[Post] = 
        POST.sendJsEntity(s"/$subjectId/posts",PostInfoJs(content)).map(_.okWithJs[Post,String](201)) 

      override val basePath: String = "/subjects"
    def apply()(using UserContext) :  PostService = 
      println("creating post service")
      PostServiceImpl()  
}



