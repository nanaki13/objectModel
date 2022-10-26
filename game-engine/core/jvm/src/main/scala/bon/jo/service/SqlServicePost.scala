package bon.jo.service

import bon.jo.sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.model.PostModel
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
import bon.jo.sql.Join3TableService
import bon.jo.domain.User
import bon.jo.domain.ImageInfo
import bon.jo.domain.UserInfo
import bon.jo.sql.Alias
import bon.jo.domain
import bon.jo.model.PostModel.{Post, PostUser}
import bon.jo.user.UserModel.toUserInfo
import java.time.ZoneId
import bon.jo.user.UserModel
import bon.jo.sql.Limit
import bon.jo.sql.JoinType
import bon.jo.image.SqlServiceImage.given
import bon.jo.sql.JoinDef
import bon.jo.sql.SqlMappings.given
import bon.jo.image.ImageModel
object SqlServicePost {

  type ServicePost = Service[Post, (Int, Long, LocalDateTime)] with SqlServicePost
  def toLocalDateTime(a: Any): LocalDateTime =
    a match
      case e: LocalDateTime => e
      case o:       java.sql.Timestamp => LocalDateTime.ofInstant( o.toInstant(),ZoneId.systemDefault())
      case o                => 
        LocalDateTime.parse(o.toString())

  given ResultSetMapping[Post] =
    (from, r) =>
      Post(idSubject = r.getInt(from),idUser = r.getLong(from +1),postDateTime = toLocalDateTime(r.getObject(from+2)),content = r.getString(from+3) )
  given ResultSetMapping[(Int, Long, LocalDateTime)] = (from, r) =>
    (r.getInt(from), r.getLong(from + 1),toLocalDateTime( r.getObject(from + 2)))
  given BaseSqlRequest[Post] = BaseSqlRequest[Post](PostModel.table)
  given BaseSqlRequest[UserInfo] = BaseSqlRequest[UserInfo](UserModel.userInfoTable)
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
  given JoinDef[Post,UserInfo] = JoinDef(JoinType.Default(), (l,r) => s"$l.${PostModel.column.idUser} = $r.${UserModel.column.id} ") 
  given JoinDef[UserInfo,ImageInfo] = JoinDef(JoinType.Left(), (l,r) => s"$l.${UserModel.column.avatarKey} = $r.${ImageModel.column.id} ") 

  val joinService = Join3TableService[Post, UserInfo,ImageInfo,JoinType.Default,JoinType.Left]()

  val idUserAlias = s"${joinService.request.t2Alias}.${UserModel.column.id}"
  val subjectAlias = s"${joinService.request.t1Alias}.${PostModel.column.idSubject}"
  def readPost(subjectId: Int,from : Long, size : Int): Seq[PostUser] = joinService
    .findAllBys((subjectAlias, subjectId))(Seq(Sort.desc(PostModel.column.postDateTime)),Limit.Fixed(from, size))
    .map{ret =>
      val (post,user,imgOpt) = ret
      PostUser(idSubject = 
        post.idSubject,user = user.copy(avatar = imgOpt),post.postDateTime,post.content)
    }
  

