package bon.jo.user

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.domain.User
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.BaseSqlRequest
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.JoinBaseSqlRequest
import bon.jo.sql.Sql.JoinService
import bon.jo.sql.Sql.JoinType
import java.sql.Connection
import bon.jo.common.Convs
import bon.jo.domain.ImageInfo
import bon.jo.image.SqlServiceImage.given
import bon.jo.sql.SqlMappings.given
import JoinBaseSqlRequest.joinRequest
import bon.jo.sql.Sql.Alias
import bon.jo.image.ImageModel
object SqlServiceUser {
  

  given JoinType.Left[User,ImageInfo] = JoinType.Left()
  given Alias = Alias()
  
  type UserWithImageService = JoinService[User,ImageInfo,JoinType.Left]
  object JoinUserImageInfoRequest extends JoinBaseSqlRequest[User,ImageInfo,JoinType.Left]
  given JoinBaseSqlRequest[User,ImageInfo,JoinType.Left] = JoinUserImageInfoRequest
  object UserWithImageService:
    inline def apply()( using ()=> Connection) :UserWithImageService = 
      val test = new JoinService[User,ImageInfo,JoinType.Left]:
        override lazy val joinCondition: String =
          s"${joinRequest.leftAlias}.${UserModel.column.avatarKey} = ${joinRequest.rightAlias}.${ImageModel.column.id}"
      type rr = test.Ret
     
      val tt : Seq[(User,Option[ImageInfo])] = test.findBys("zf"->"")()
      test
  
  
    
  type ServiceUser = Service[User,Long] with SqlServiceUser
  given BaseSqlRequest[User] = BaseSqlRequest[User](UserModel.userTable)
  given ResultSetMapping[User] with
    def apply(from : Int,r : ResultSet):User = 
      User(r.getLong(from),r.getString(from+1),r.getString(from+2), Option(r.getObject(from+3)).map(Convs.conv))

  given PSMapping[User] with
     def apply(from : Int,v : User)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      stmtSetObject(from+2,v.pwd)
      v.avatarKey match
        case Some(iKey) => 
          stmtSetObject(from+3,iKey)
        case _ => 
          stmtSetObject(from+3,null)
      
      from+4

  inline def apply()( using ()=> Connection) : ServiceUser = new Service[User,Long] with SqlServiceUser
  

}
trait SqlServiceUser:
  self :  Service[User,Long] =>
    def find(name : String) :Option[User] = 
      self.findBy(UserModel.column.name,name)
    def containsName(name : String): Boolean = 
      self.contains(UserModel.column.name -> name)

