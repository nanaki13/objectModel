package bon.jo.service

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors

import ScoreRepo.Command.*
import ScoreRepo.Response.*
import bon.jo.service.SqlServiceScore.ServiceScore

import bon.jo.model.ScoreModel.Score
import bon.jo.domain.GameLevel
import bon.jo.domain.UserScore
object ScoreRepo {


  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  AddScore(score: Score, replyTo: ActorRef[Response]) extends Command
    case  ReadScores(gameLevel: GameLevel, replyTo: ActorRef[Seq[UserScore]]) extends Command


  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(scores: ServiceScore): Behavior[Command] = Behaviors.receiveMessage {
    m => 
      m match
        case AddScore(score, replyTo) =>
          if scores.updateScore(score) then
            replyTo ! Response.OK
          else
            replyTo ! Response.KO("Not the best")
          
        case ReadScores(gameLevel, replyTo) =>
          val score = scores.readScore(gameLevel.idGame,gameLevel.lvl)
          replyTo ! score

        Behaviors.same
  }
}