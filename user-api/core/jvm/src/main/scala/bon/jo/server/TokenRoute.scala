package bon.jo.server

import akka.actor.typed.ActorSystem
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.server.Route

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor.typed.ActorRef
import bon.jo.user.UserRepo
import bon.jo.domain.User
import bon.jo.domain.UserAvatar
import bon.jo.domain.UserInfo
import bon.jo.user.UserRepo.Command
import bon.jo.domain.Response
import bon.jo.user.TokenRepo
import org.json4s.Formats
import scala.concurrent.ExecutionContext
import bon.jo.domain.UserLogin
import pdi.jwt.JwtClaim.apply
import pdi.jwt.JwtClaim
import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
import akka.actor.typed.scaladsl.AskPattern.Askable
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import akka.http.scaladsl.server.StandardRoute.toDirective
import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.server.AuthorizationFailedRejection
import com.github.t3hnar.bcrypt._
import bon.jo.user.UserModel.toUserInfo
import bon.jo.user.TokenRepo.userFromDb

class TokenRoutes()(using userRepo: ActorRef[UserRepo.Command],tokenRepo: ActorRef[TokenRepo.Command],sys:   ActorSystem[_], f: Formats,ec :ExecutionContext,t :Timeout) extends  CORSHandler with TokenRouteGuard(using tokenRepo) {

  //object userJson  extends JsonSupport[User] 
  val userJson : JsonSupport[UserLogin] = JsonSupport[UserLogin]()
  import userJson.given
  // asking someone requires a timeout and a scheduler, if the timeout hits without response


  lazy val theTokenRoutes: Route =
    corsHandler( pathPrefix("token") {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[UserLogin]) { user =>
                 val operationPerformed: Future[Option[String]] = userRepo.ask[Option[UserAvatar]](UserRepo.Command.FindUsers(user.name,_)).flatMap{ e =>
                      e match
                        case Some(u) if user.pwd.isBcryptedBounded(u.user.pwd) => 
                          tokenRepo.ask[String](TokenRepo.Command.GetToken(u.toUserInfo,_)).map(e => Some(e))
                        case _ => Future(None)
                 }
                 onSuccess(operationPerformed) {
                      case Some(u) => complete(u)  
                      case _ => complete(StatusCodes.Unauthorized -> "invalid credantial")
                    
                }

              }
            }
          )
        },
        (get & guard & path("refresh")){
          claim => 
            val f : Future[UserAvatar] = claim.userFromDb
            val getToken = f.flatMap{
              user => tokenRepo.ask[String](TokenRepo.Command.GetToken(user.toUserInfo,_))
            }
            onSuccess(getToken)(complete)

        }
      )
    })
    
}
trait TokenRouteGuard(using tokenRepo: ActorRef[TokenRepo.Command],s :ActorSystem[_], t:  Timeout, ec : ExecutionContext   ):

    def guard : Directive1[JwtClaim] = {
    optionalHeaderValueByName(Authorization.name).flatMap{
      case Some(auth ) => 
        val tokenEx = "Bearer ([^\\s]*)".r
        auth match 
          case tokenEx(token) => 
            onSuccess( tokenRepo.ask[Try[JwtClaim]](TokenRepo.Command.ParseToken(token,_))).flatMap{
              case Success(t) => provide(t)
              case Failure(t) =>reject(AuthorizationFailedRejection)
            }
          case _ => reject(AuthorizationFailedRejection)
        
      case None => reject(AuthorizationFailedRejection)

    }
  }