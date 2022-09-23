package bon.jo.server

import akka.actor.typed.ActorSystem
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor.typed.ActorRef
import bon.jo.user.UserRepo
import bon.jo.user.UserModel.User
import bon.jo.user.UserModel.UserInfo
import bon.jo.user.UserRepo.Command
import bon.jo.user.UserRepo.Response
import bon.jo.user.TokenRepo
import org.json4s.Formats
import scala.concurrent.ExecutionContext
import bon.jo.user.UserModel.UserLogin

class TokenRoutes(userRepo: ActorRef[UserRepo.Command],tokenRepo: ActorRef[TokenRepo.Command])(using   ActorSystem[_],Formats) extends  CORSHandler  {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable

  given ExecutionContext = summon[ActorSystem[_]].executionContext
  //object userJson  extends JsonSupport[User] 
  val userJson : JsonSupport[UserLogin] = JsonSupport[UserLogin]()
  import userJson.given
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  given Timeout = 3.seconds

  lazy val theTokenRoutes: Route =
    corsHandler( pathPrefix("token") {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[UserLogin]) { user =>
                 val operationPerformed: Future[Option[String]] = userRepo.ask[Option[User]](UserRepo.Command.FindUsers(user.name,_)).flatMap{ e =>
                      e match
                        case Some(u) if u.pwd == user.pwd => 
                          tokenRepo.ask[String](TokenRepo.Command.GetToken(UserInfo(u.id,u.name),_)).map(e => Some(e))
                        case _ => Future(None)
                 }
                 onSuccess(operationPerformed) {
                      case Some(u) => complete(u)  
                      case _ => complete(StatusCodes.Unauthorized -> "invalid credantial")
                    
                }

              }
            }
          )
        }
      )
    })
}