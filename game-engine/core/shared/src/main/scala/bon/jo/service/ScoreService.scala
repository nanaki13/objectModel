package bon.jo.service

import scala.concurrent.Future

import bon.jo.domain.ScoreInfo
import bon.jo.domain.UserScore

trait ScoreService :
  def getScores(lvl : Int) : Future[Seq[UserScore]]
  def saveScore(s : ScoreInfo) : Future[SaveResult]
  def getScore(pseudo : String):Future[Option[ScoreInfo]]
