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
import bon.jo.domain.User
import bon.jo.domain.ImageSend
import bon.jo.user.UserModel.toUserInfo
import bon.jo.user.UserRepo.Command
import bon.jo.user.UserRepo.Response
import org.json4s.Formats
import scala.concurrent.ExecutionContext
import bon.jo.domain.UserInfo
import bon.jo.domain.UserLogin
import bon.jo.user.TokenRepo
import bon.jo.user.TokenRepo.{toUserInfo as toUi}
import bon.jo.image.ImageRepo
import bon.jo.domain.Image

class UserRoutes(buildUserRepository: ActorRef[UserRepo.Command])(using
    ActorRef[TokenRepo.Command],
    ActorRef[ImageRepo.Command],
    ActorSystem[_],
    Manifest[User],
    Manifest[Seq[User]],
    Formats
) extends JsonSupport[User]
    with CORSHandler
    with TokenRouteGuard {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  given ec: ExecutionContext = summon[ActorSystem[_]].executionContext
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  val userInfoJson: JsonSupport[UserInfo] = JsonSupport[UserInfo]()
  import userInfoJson.given
  val userLoginJson: JsonSupport[UserLogin] = JsonSupport[UserLogin]()
  import userLoginJson.given
  inline def imageRepo = summon[ActorRef[ImageRepo.Command]]
  given t: Timeout = 3.seconds

  lazy val theUserRoutes: Route =
    corsHandler {
      pathPrefix("users") {
        concat(
          pathEnd {
            concat(
              post {
                entity(as[UserLogin]) { user =>
                  val operationPerformed: Future[UserRepo.Response] =
                    buildUserRepository.ask(Command.AddUser(user, _))
                  onSuccess(operationPerformed) {
                    case Response.OK => complete("User added")
                    case Response.KO(reason) =>
                      complete(StatusCodes.Conflict -> reason)
                  }
                }
              },
              get {
                parameter("name") { name =>
                  rejectEmptyResponse {
                    val maybeUser: Future[Option[User]] =
                      buildUserRepository.ask(Command.FindUsers(name, _))
                    complete(maybeUser.map(_.map(_.toUserInfo)))
                  }
                }
              }
            )
          },
         (post & path("avatar")){
            (guard &  ImageRoutes. extractData("image")) { (claim,data) => 
              val ui = claim.toUi
              val imgName = s"${ui.name}_avatar"
              val createInDb = imageRepo.ask(ImageRepo.Command.AddImage(ImageSend(imgName,data),_)).map(_ => imgName)

              val ret : Future[Option[Image]] = for{
                fName <- imageRepo.ask(ImageRepo.Command.AddImage(ImageSend(imgName,data),_)).map{
                    case ImageRepo.Response.OK => imgName
                    case ImageRepo.Response.KO(reason) =>
                      throw IllegalStateException(reason)
                  }
                imgDb <- imageRepo.ask[Option[Image]](ImageRepo.Command.FindImages(fName,_))
                userUpa <- buildUserRepository.ask
              } yield imgDb
              onSuccess(createInDb) {
                    case ImageRepo.Response.OK => complete("Image created")
                    case ImageRepo.Response.KO(reason) =>
                      complete(StatusCodes.InternalServerError -> reason)
                  }
            }
          
            
          }
          ,
          (delete & path(LongNumber)) { id =>
            guard { claim =>
              val ui = claim.toUi
              ui.name match
                case "Nanaki" =>
                  val operationPerformed: Future[Response] =
                    buildUserRepository.ask(Command.ClearUsers(id, _))
                  onSuccess(operationPerformed) {
                    case Response.OK => complete("Users cleared")
                    case Response.KO(reason) =>
                      complete(StatusCodes.InternalServerError -> reason)
                  }
                case o => complete(StatusCodes.Forbidden)
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

}
