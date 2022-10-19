package bon.jo.pong.service

import bon.jo.pong.Login.UserContext
import bon.jo.service.ScoreService
import bon.jo.request.HttpRequest
import bon.jo.request.HttpRequest.Method.{POST, GET}
import bon.jo.pong.HttpServiceConfig.AuthParam.given
import scala.concurrent.Future
import bon.jo.service.SaveResult
import bon.jo.domain.ScoreInfo
import bon.jo.pong.Login.given
import concurrent.ExecutionContext.Implicits.global
import scalajs.js
import bon.jo.domain.UserScore
import bon.jo.pong.Login
import bon.jo.domain.UserInfo
import bon.jo.common.Anys.toSome
import bon.jo.domain.Score
import bon.jo.request.HttpRequest.given

object ScoreServiceRest {

    trait ScoreInfoJs extends js.Object:
      val idGame: Int
      val lvl: Int
      val scoreValue: Double
    trait ScoreJs extends ScoreInfoJs:
      val idUser: Int
      val scoreDateTime : String
      val scoreValue : Double
    trait UserScoreJs extends js.Object:
      val user: Login.UserInfoJs
      val score : ScoreJs
    object UserScoreJsParser:
      def unapply(u : UserScoreJs) : Some[UserScore] = 
        UserScore(Login.userInfo(u.user),Score(u.score.idGame,u.score.lvl,u.score.idUser.toLong, u.score.scoreDateTime,u.score.scoreValue.toLong)).toSome
    object ScoreInfoJs:
      def apply(score : ScoreInfo): ScoreInfoJs =
        js.Dynamic.literal(idGame = score.idGame, lvl = score.lvl, scoreValue = score.scoreValue.toDouble).asInstanceOf
    given Conversion[js.Any,UserScore] = 
      e => 
        val UserScoreJsParser(userScore) = e.asInstanceOf[UserScoreJs]
        userScore
    given Conversion[String,String] = e=>e 
    class ScoreServiceImpl()(using UserContext) extends ScoreService with HttpRequest.Service :

      override def getScores(): Future[Seq[UserScore]] = 
        GET.sendOn("?idGame=1&lvl=1").map(_.okWithJs[Seq[UserScore],String](200)) 

      override def saveScore(s: ScoreInfo): Future[SaveResult] = 
        POST.sendJsEntity("", ScoreInfoJs(s)).map(_.mapStatus{
          case 201 => SaveResult.Updated 
          case 409 => SaveResult.NotUpdated 
        }) 

      override def getScore(pseudo: String): Future[Option[ScoreInfo]] = ???

      override val basePath: String = "/scores"
    def apply()(using UserContext) :  ScoreService = 
      ScoreServiceImpl()  
}



