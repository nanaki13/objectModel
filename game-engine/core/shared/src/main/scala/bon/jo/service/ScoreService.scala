package bon.jo.service

import scala.concurrent.Future
import ScoreService.*

trait ScoreService :
  def getScores() : Future[Seq[Score]]
  def saveScore(s : Score) : Future[SaveResult]
  def getScore(pseudo : String):Future[Option[Score]]

object ScoreService:
  enum SaveResult:
    case New,Updated,NotUpdated
  case class Score(value : Int,owner : String)
