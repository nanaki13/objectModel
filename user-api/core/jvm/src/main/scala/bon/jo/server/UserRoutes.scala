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
import bon.jo.domain.UserAvatar
import bon.jo.domain.ImageSend
import bon.jo.user.UserModel.toUserInfo
import bon.jo.user.UserRepo.Command
import bon.jo.domain.Response
import bon.jo.domain.Response.*
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
    Formats,
    Timeout,
    ExecutionContext
) extends JsonSupport[User]
    with CORSHandler
    with TokenRouteGuard {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable

  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  val userInfoJson: JsonSupport[UserInfo] = JsonSupport[UserInfo]()
  import userInfoJson.given
  val userLoginJson: JsonSupport[UserLogin] = JsonSupport[UserLogin]()
  import userLoginJson.given
  inline def imageRepo = summon[ActorRef[ImageRepo.Command]]


  lazy val theUserRoutes: Route =
    corsHandler {
      pathPrefix("users") {
        concat(
          pathEnd {
            concat(
              post {
                entity(as[UserLogin]) { user =>
                  val operationPerformed: Future[Response] =
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
                    val maybeUser: Future[Option[UserAvatar]] =
                      buildUserRepository.ask(Command.FindUsers(name, _))
                    complete(maybeUser.map(_.map(_.toUserInfo)))
                  }
                }
              }
            )
          },
         (post & path("avatar")){
            (guard &  ImageRoutes.extractData("image")) { (claim,contentType,data) => 
              val ui = claim.toUi
              val imgName = s"${ui.name}_avatar.$contentType"
              def createInDb(user : User):Future[Response] = 
                user.avatarKey match
                  case None =>       
                    imageRepo.ask(ImageRepo.Command.AddImage(ImageSend(imgName,data),_)).flatMap{
                      case Response.OK => imageRepo.ask[Option[Image]](ImageRepo.Command.FindImages(imgName,_)).map(_.getOrElse(throw new IllegalStateException(s"no image $imgName")))  
                      case Response.KO(reason) =>
                        throw IllegalStateException(reason) 
                    }.map(_.id).flatMap{ imgId => 
                         buildUserRepository.ask[Response](Command.UpdateUser(user.copy(avatarKey = Some(imgId)),_))
                    }
              
                  case Some(value) => 
                    imageRepo.ask[Response](ImageRepo.Command.UpdateImage(Image(value,imgName,data),_))


              val ret : Future[Response] = (for{
                userTuoUp <- buildUserRepository.ask[Option[UserAvatar]](Command.FindUsers(ui.name, _)).map(_.getOrElse(throw new IllegalStateException(s"no user ${ui.name}")))
                update <- createInDb(userTuoUp.user)
                
              } yield (update))
              onSuccess(ret) {
                    case Response.OK => complete("Avatar Updated")
                    case Response.KO(reason) =>
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
            val maybeUser: Future[Option[UserAvatar]] =
              buildUserRepository.ask(Command.GetUserById(id, _))
            rejectEmptyResponse {
              complete(maybeUser.map(_.map(_.toUserInfo)))
            }
          }
        )
      }
    }

}
