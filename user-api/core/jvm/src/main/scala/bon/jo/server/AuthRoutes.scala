package bon.jo.server

import akka.actor.typed.ActorSystem
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor.typed.ActorRef
import _root_.bon.jo.user.UserRepo
import _root_.bon.jo.user.UserModel.User
import _root_.bon.jo.user.UserRepo.Command
import _root_.bon.jo.user.UserRepo.Response
import org.json4s.Formats
import bon.jo.server.Req.UserLogin
import org.json4s.JObject
import pdi.jwt.{JwtJson4s, JwtAlgorithm}, org.json4s._, org.json4s.JsonDSL.WithBigDecimal._
import java.time.Clock
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset

class AuthRoutes(buildUserRepository: ActorRef[UserRepo.Command])(using
    ActorSystem[_],
    Manifest[User],
    Manifest[Seq[User]],
    Manifest[UserLogin],
    Manifest[Seq[UserLogin]],
    Formats
) extends JsonSupport[User] with CORSHandler {

  val jsSupportLogin = new JsonSupport[UserLogin] {}
  
  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  given Timeout = 3.seconds

  lazy val theAuthRoutes: Route =
    import jsSupportLogin.given
    corsHandler{
      pathPrefix("auth") {
        concat(
          pathEnd {
            concat(
              post {
                entity(as[UserLogin]) { userReq =>
                  val maybeUser: Future[Option[User]] =
                    buildUserRepository.ask(Command.FindUsers(userReq.name, _))
                  onSuccess(maybeUser) {
                    case Some(user) if user.pwd == userReq.pwd => 
                      
                      
                      val claim = JObject(("user", JObject("name" -> string2jvalue( user.name))), ("nbf", -1), ("exp", LocalDateTime.now().plusDays(5).toEpochSecond(ZoneOffset.UTC)))
                      val key = "efesfvevsrthrtdfgrwdrgdrg"
                      val algo = JwtAlgorithm.HS256
                      val token = JwtJson4s.encode(claim, key, algo)
                      complete(StatusCodes.OK -> token)  
                    case _ =>
                      complete(StatusCodes.Unauthorized)
                  }
                }
              }
            )
          }
        )
      }
    } 
    

}
