package bon.jo.service

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors

import ServiceRepo.Command.*
import ServiceRepo.Response.*
import bon.jo.sql.Sql
import bon.jo.service.SqlServiceScore.ServiceScore

import bon.jo.model.ScoreModel.Score
import bon.jo.model.ScoreModel.GameLevel
object ServiceRepo {


  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  AddScore(score: Score, replyTo: ActorRef[Response]) extends Command
    case  ReadScores(gameLevel: GameLevel, replyTo: ActorRef[Seq[Score]]) extends Command


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
          replyTo ! scores.readScore(gameLevel.idGame,gameLevel.lvl)

        Behaviors.same
  }
}