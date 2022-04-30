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

class UserRoutes(buildUserRepository: ActorRef[UserRepo.Command])(using   ActorSystem[_],Manifest[User],Manifest[Seq[User]],Formats) extends JsonSupport[User] {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable

  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  given Timeout = 3.seconds

  lazy val theUserRoutes: Route =
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[User]) { user =>
                val operationPerformed: Future[UserRepo.Response] =
                  buildUserRepository.ask(Command.AddUser(user, _))
                onSuccess(operationPerformed) {
                  case Response.OK         => complete("User added")
                  case Response.KO(reason) => complete(StatusCodes.InternalServerError -> reason)
                }
              }
            },
            get {
              parameter("name") {
                name => 
                rejectEmptyResponse {
                  val maybeUser: Future[Option[User]] = buildUserRepository.ask(Command.FindUsers(name, _))
                  complete(maybeUser)
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