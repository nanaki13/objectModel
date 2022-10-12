package bon.jo.service

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.model.ScoreModel
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.Sort
import bon.jo.sql.Sql.Sort.{asc,desc}
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.stmt
import java.sql.Connection
import java.time.LocalDateTime
import bon.jo.sql.Sql.BaseSqlRequest
import bon.jo.sql.Sql.JoinBaseSqlRequest
import bon.jo.sql.Sql.JoinService
import bon.jo.domain.User
import bon.jo.sql.Sql.Alias
import bon.jo.domain.UserScore
import bon.jo.domain
import bon.jo.model.ScoreModel.Score
import bon.jo.user.UserModel.toUserInfo
import java.time.ZoneId
import bon.jo.sql.Sql.Limit
object SqlServiceScore {

  type ServiceScore = Service[Score, (Int, Int, Long)] with SqlServiceScore
  def toLocalDateTime(a: Any): LocalDateTime =
    a match
      case e: LocalDateTime => e
      case o:       java.sql.Timestamp => LocalDateTime.ofInstant( o.toInstant(),ZoneId.systemDefault())
      case o                => 
        println(o.getClass())
        LocalDateTime.parse(o.toString())

  given ResultSetMapping[Score] =
    (from, r) =>
      Score(
        r.getInt(from),
        r.getInt(from + 1),
        r.getLong(from + 2),
        toLocalDateTime(r.getObject(from + 3)),
        r.getLong(from + 4)
      )
  given ResultSetMapping[(Int, Int, Long)] = (from, r) =>
    (r.getInt(from), r.getInt(from + 1), r.getLong(from + 2))
  given BaseSqlRequest[Score] = BaseSqlRequest[Score](ScoreModel.table)
  given PSMapping[Score] with
    def apply(from: Int, v: Score)(using PreparedStatement): Int =
      stmtSetObject(from, v.idGame)
      stmtSetObject(from + 1, v.lvl)
      stmtSetObject(from + 2, v.idUser)
      stmtSetObject(from + 3, v.scoreDateTime)
      stmtSetObject(from + 4, v.scoreValue)
      from + 5
  given PSMapping[(Int, Int, Long)] with
    def apply(from: Int, v: (Int, Int, Long))(using PreparedStatement): Int =
      stmtSetObject(from, v._1)
      stmtSetObject(from + 1, v._2)
      stmtSetObject(from + 2, v._3)
      println(stmt)
      from + 3
  inline def apply()(using() => Connection): ServiceScore =
    new Service[Score, (Int, Int, Long)] with SqlServiceScore

}
trait SqlServiceScore:
  self: Service[Score, (Int, Int, Long)] =>
  import bon.jo.user.SqlServiceUser.given
  import SqlServiceScore.given
  import self.given
  given (() => Connection) = connection
  given Alias = Alias()
  given joinRequest: JoinBaseSqlRequest[User, Score] =
    new JoinBaseSqlRequest[User, Score]() {}
  val idUserAlias = s"${joinRequest.leftAlias}.id"
  val lvlAlias = s"${joinRequest.rightAlias}.${ScoreModel.cLvl}"
  val gameIdAlias = s"${joinRequest.rightAlias}.${ScoreModel.cIdGame}"
  object joinService extends JoinService[User, Score]:
    override lazy val joinCondition: String =
      s"${joinRequest.leftAlias}.id = ${joinRequest.rightAlias}.id_user"

  def readScore(gameId: Int, level: Int): Seq[UserScore] = joinService
    .findBys((gameIdAlias, gameId), (lvlAlias, level))(Seq(Sort.desc(ScoreModel.cScore),Sort.asc(ScoreModel.cScoreDate)))
    .map((u, s) =>
      UserScore(
        u.toUserInfo,
        domain.Score(
          s.idGame,
          s.lvl,
          s.idUser,
          s.scoreDateTime.toString,
          s.scoreValue
        )
      )
    )
  def readScore(gameId: Int, level: Int, userId: Long): Option[Score] =
    readOption(gameId, level, userId)
  inline def readScore(score: Score): Option[Score] =
    readScore(score.idGame, score.lvl, score.idUser)
  def updateScore(score: Score): Boolean =
    def createF(): Boolean =
      create(score)
      true
    def updateF(): Boolean =
      update((score.idGame, score.lvl, score.idUser), score)
      true
    readScore(score).map(_.scoreValue) match
      case Some(scorePrevious) if scorePrevious < score.scoreValue => updateF()
      case None                                                    => createF()
      case _                                                       => false
