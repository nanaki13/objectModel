package bon.jo.route

import akka.actor.typed.ActorSystem
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor.typed.ActorRef
import org.json4s.Formats
import scala.concurrent.ExecutionContext
import bon.jo.user.TokenRepo
import bon.jo.user.TokenRepo.{toUserInfo as toUi}
import bon.jo.server.JsonSupport
import bon.jo.server.CORSHandler
import bon.jo.server.TokenRouteGuard
import bon.jo.service.PostRepo
import bon.jo.service.PostRepo.Command
import bon.jo.service.PostRepo.Response
import bon.jo.domain.PostInfo
import bon.jo.domain.GameLevel
import bon.jo.model.PostModel.{Post, PostUser}
import java.time.LocalDateTime


class PostRoutes(buildUserRepository: ActorRef[PostRepo.Command])(using
    ActorRef[TokenRepo.Command],
    ActorSystem[_],
    Formats,ExecutionContext,Timeout
) extends CORSHandler
    with TokenRouteGuard {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  val postJson: JsonSupport[Post] = JsonSupport[Post]()
  val postInfoJson: JsonSupport[PostInfo] = JsonSupport[PostInfo]()
  val postUserJson: JsonSupport[PostUser] = JsonSupport[PostUser]()
  import postJson.given
  import postUserJson.given
  import postInfoJson.given

  lazy val route: Route = corsHandler {
    pathPrefix("subjects") {
      concat(
        
        path(IntNumber / "posts") { subjectId =>
          concat(
            post {
              guard { calim =>
                val user = calim.toUi
                entity(as[PostInfo]) { post =>
                  val newPost = Post(
                    idSubject = subjectId,
                    idUser = user.id,
                    postDateTime = LocalDateTime.now(),
                    content = post.content
                  )
                  val operationPerformed: Future[PostRepo.Response] =
                    buildUserRepository.ask(
                      Command.AddPost(
                        newPost,
                        _
                      )
                    )
                  onSuccess(operationPerformed) {
                    case Response.OK =>
                      complete(StatusCodes.Created, newPost)
                    case Response.KO(reason) =>
                      complete(StatusCodes.Conflict -> reason)
                  }
                }

              }

            },
            get {
              parameter("from".as[Long], "size".as[Int]) { (from, size) =>

                val maybeUser: Future[Seq[PostUser]] =
                  buildUserRepository.ask(
                    Command.ReadPosts(subjectId, from, size, _)
                  )
                onSuccess(maybeUser)(complete)

              }
            }
          )
        }
      )
    }

  }
}
