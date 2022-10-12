package bon.jo.service

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.model.PostModel
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
import bon.jo.domain
import bon.jo.model.PostModel.{Post, PostUser}
import bon.jo.user.UserModel.toUserInfo
import java.time.ZoneId
import bon.jo.user.UserModel
import bon.jo.sql.Sql.Limit
object SqlServicePost {

  type ServicePost = Service[Post, (Int, Long, LocalDateTime)] with SqlServicePost
  def toLocalDateTime(a: Any): LocalDateTime =
    a match
      case e: LocalDateTime => e
      case o:       java.sql.Timestamp => LocalDateTime.ofInstant( o.toInstant(),ZoneId.systemDefault())
      case o                => 
        println(o.getClass())
        LocalDateTime.parse(o.toString())

  given ResultSetMapping[Post] =
    (from, r) =>
      Post(idSubject = r.getInt(from),idUser = r.getLong(from +1),postDateTime = toLocalDateTime(r.getObject(from+2)),content = r.getString(from+3) )
  given ResultSetMapping[(Int, Long, LocalDateTime)] = (from, r) =>
    (r.getInt(from), r.getLong(from + 1),toLocalDateTime( r.getObject(from + 2)))
  given BaseSqlRequest[Post] = BaseSqlRequest[Post](PostModel.table)
  given PSMapping[Post] with
    def apply(from: Int, v: Post)(using PreparedStatement): Int =
      stmtSetObject(from, v.idSubject)
      stmtSetObject(from + 1, v.idUser)
      stmtSetObject(from + 2, v.postDateTime)
      stmtSetObject(from + 3, v.content)
      from + 4
  given PSMapping[(Int, Long, LocalDateTime)] with
    def apply(from: Int, v: (Int, Long, LocalDateTime))(using PreparedStatement): Int =
      stmtSetObject(from, v._1)
      stmtSetObject(from + 1, v._2)
      stmtSetObject(from + 2, v._3)
      println(stmt)
      from + 3
  inline def apply()(using() => Connection): ServicePost =
    new Service[Post, (Int, Long, LocalDateTime)] with SqlServicePost

}
trait SqlServicePost:
  self: Service[Post, (Int, Long, LocalDateTime)] =>
  import bon.jo.user.SqlServiceUser.given
  import SqlServicePost.given
  import self.given
  given (() => Connection) = connection
  given Alias = Alias()
  given joinRequest: JoinBaseSqlRequest[Post, User] =
    new JoinBaseSqlRequest[Post, User]() {}
  val idUserAlias = s"${joinRequest.rightAlias}.${UserModel.column.id}"
  val subjectAlias = s"${joinRequest.leftAlias}.${PostModel.column.idSubject}"

  object joinService extends JoinService[Post, User]:
    override lazy val joinCondition: String =
      s"${joinRequest.leftAlias}.${PostModel.column.idUser} = ${joinRequest.rightAlias}.${UserModel.column.id}"

  def readPost(subjectId: Int,from : Long, size : Int): Seq[PostUser] = joinService
    .findBys((subjectAlias, subjectId))(Seq(Sort.desc(PostModel.column.postDateTime)),Limit.Fixed(from, size))
    .map((post, user) =>
      PostUser(idSubject = 
        post.idSubject,user = user.toUserInfo,post.postDateTime,post.content)
    )
  

