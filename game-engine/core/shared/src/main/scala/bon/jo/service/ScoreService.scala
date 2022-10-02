package bon.jo.service

import scala.concurrent.Future
import ScoreService.*
import bon.jo.domain.ScoreInfo
import bon.jo.domain.UserScore

trait ScoreService :
  def getScores() : Future[Seq[UserScore]]
  def saveScore(s : ScoreInfo) : Future[SaveResult]
  def getScore(pseudo : String):Future[Option[ScoreInfo]]

object ScoreService:
  enum SaveResult:
    case Updated,NotUpdated
