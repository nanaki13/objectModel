package bon.jo.service

import bon.jo.sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.model.ScoreModel
import java.sql.PreparedStatement
import bon.jo.sql.Service
import bon.jo.sql.Sort
import bon.jo.sql.Sort.{asc,desc}
import bon.jo.sql.PSMapping
import bon.jo.sql.stmtSetObject
import bon.jo.sql.stmt
import java.sql.Connection
import java.time.LocalDateTime
import bon.jo.sql.BaseSqlRequest
import bon.jo.sql.Join2TableRequest
import bon.jo.sql.Join2TableService
import bon.jo.domain.User
import bon.jo.sql.Alias
import bon.jo.domain.UserScore
import bon.jo.domain
import bon.jo.model.ScoreModel.Score
import bon.jo.model.ScoreModel.toDomain
import bon.jo.user.UserModel.toUserInfo
import java.time.ZoneId
import bon.jo.sql.Limit
import bon.jo.sql.JoinType
import bon.jo.sql.JoinDef
import bon.jo.domain.UserInfo
import bon.jo.user.UserModel
import bon.jo.domain.ImageInfo
import bon.jo.image.ImageModel
import bon.jo.sql.Join3TableRequest
import bon.jo.sql.Join3TableService
import bon.jo.sql.SqlMappings.given
object SqlServiceScore {

  type ServiceScore = Service[Score, (Int, Int, Long)] with SqlServiceScore
  def toLocalDateTime(a: Any): LocalDateTime =
    a match
      case e: LocalDateTime => e
      case o:       java.sql.Timestamp => LocalDateTime.ofInstant( o.toInstant(),ZoneId.systemDefault())
      case o                => 
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
      from + 3
  inline def apply()(using() => Connection): ServiceScore =
    new Service[Score, (Int, Int, Long)] with SqlServiceScore

}
trait SqlServiceScore:
  self: Service[Score, (Int, Int, Long)] =>
  import bon.jo.user.SqlServiceUser.given
  import SqlServiceScore.given
  import self.given
  import bon.jo.service.SqlServicePost.given
  import bon.jo.image.SqlServiceImage.given
  given (() => Connection) = connection
  given Alias = Alias()
  given JoinDef[Score,UserInfo] = JoinDef(JoinType.Default(), (l,r) => s"$l.${ScoreModel.cIdUser} = $r.${UserModel.column.id} ") 
  given JoinDef[UserInfo,ImageInfo] = JoinDef(JoinType.Left(), (l,r) => s"$l.${UserModel.column.avatarKey} = $r.${ImageModel.column.id} ") 

  val joinService = Join3TableService[Score, UserInfo,ImageInfo,JoinType.Default,JoinType.Left]()


  val idUserAlias = s"${joinService.request.t2Alias}.${UserModel.column.id}"
  val lvlAlias = s"${joinService.request.t1Alias}.${ScoreModel.cLvl}"
  val gameIdAlias = s"${joinService.request.t1Alias}.${ScoreModel.cIdGame}"
  def readScore(gameId: Int, level: Int): Seq[UserScore] = joinService
    .findAllBys((gameIdAlias, gameId), (lvlAlias, level))(Seq(Sort.desc(ScoreModel.cScore),Sort.asc(ScoreModel.cScoreDate)))
    .map{case (s, u, i) =>  
      UserScore(
        u.copy(avatar = i),
        s.toDomain()
      )
    }
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
