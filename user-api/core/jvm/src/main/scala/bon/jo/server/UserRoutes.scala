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
import bon.jo.user.UserModel.toUserInfo
import bon.jo.user.UserRepo.Command
import bon.jo.user.UserRepo.Response
import org.json4s.Formats
import scala.concurrent.ExecutionContext
import bon.jo.user.UserModel.UserInfo
import bon.jo.user.UserModel.UserLogin

class UserRoutes(buildUserRepository: ActorRef[UserRepo.Command])(using   ActorSystem[_],Manifest[User],Manifest[Seq[User]],Formats) extends JsonSupport[User] with CORSHandler {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  given ExecutionContext = summon[ActorSystem[_]].executionContext
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  val userInfoJson : JsonSupport[UserInfo] = JsonSupport[UserInfo]()
  import userInfoJson.given
   val userLoginJson : JsonSupport[UserLogin] = JsonSupport[UserLogin]()
  import userLoginJson.given
  given Timeout = 3.seconds

  lazy val theUserRoutes: Route =
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            corsHandler(post {
              entity(as[UserLogin]) { user =>
                val operationPerformed: Future[UserRepo.Response] =
                  buildUserRepository.ask(Command.AddUser(user, _))
                onSuccess(operationPerformed) {
                  case Response.OK         => complete("User added")
                  case Response.KO(reason) => complete(StatusCodes.InternalServerError -> reason)
                }
              }
            }),
            get {
              parameter("name") {
                name => 
                rejectEmptyResponse {
                  val maybeUser: Future[Option[User]] = buildUserRepository.ask(Command.FindUsers(name, _))
                  complete(maybeUser.map(_.map(_.toUserInfo)))
                }
              }
            }
          )
        },
        (delete& path(LongNumber)) { id =>
          val operationPerformed: Future[Response] =
            buildUserRepository.ask(Command.ClearUsers(id,_))
          onSuccess(operationPerformed) {
            case Response.OK         => complete("Users cleared")
            case Response.KO(reason) => complete(StatusCodes.InternalServerError -> reason)
          }
        },
        
        (get & path(LongNumber)) { id =>
          val maybeUser: Future[Option[User]] =
            buildUserRepository.ask(Command.GetUserById(id, _))
          rejectEmptyResponse {
            complete(maybeUser)
          }
        }
      )
    }
}