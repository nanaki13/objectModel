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
import bon.jo.service.ScoreRepo
import bon.jo.service.ScoreRepo.Command
import bon.jo.service.ScoreRepo.Response
import bon.jo.domain.ScoreInfo
import bon.jo.domain.GameLevel
import bon.jo.model.ScoreModel.Score
import java.time.LocalDateTime
import akka.util.Timeout
import bon.jo.domain.UserScore

class ScoreRoutes(buildUserRepository: ActorRef[ScoreRepo.Command])(using
    ActorRef[TokenRepo.Command],
    ActorSystem[_],
    Formats,Timeout,ExecutionContext
) extends JsonSupport[ScoreInfo]
    with CORSHandler
    with TokenRouteGuard {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  val scoreJson: JsonSupport[Score] = JsonSupport[Score]()
  val scoreUserJson: JsonSupport[UserScore] = JsonSupport[UserScore]()
  import scoreJson.given
  import scoreUserJson.given

  lazy val route: Route = corsHandler {
    pathPrefix("scores") {
      concat(
        pathEnd {
          concat(
            post {
              guard { calim =>
                val user = calim.toUi
                entity(as[ScoreInfo]) { score =>
                  val operationPerformed: Future[ScoreRepo.Response] =
                    buildUserRepository.ask(
                      Command.AddScore(
                        Score(
                          score.idGame,
                          score.lvl,
                          user.id,
                          LocalDateTime.now(),
                          score.scoreValue
                        ),
                        _
                      )
                    )
                  onSuccess(operationPerformed) {
                    case Response.OK =>
                      complete(StatusCodes.Created, "Score added")
                    case Response.KO(reason) =>
                      complete(StatusCodes.Conflict -> reason)
                  }
                }

              }

            },
            get {
              parameter("idGame".as[Int], "lvl".as[Int]) { (idGame, lvl) =>
                rejectEmptyResponse {
                  val maybeUser: Future[Seq[UserScore]] =
                    buildUserRepository.ask(
                      Command.ReadScores(GameLevel(idGame, lvl), _)
                    )
                  onSuccess(maybeUser)(complete)
                }
              }
            }
          )
        }
      )
    }
  }

}
