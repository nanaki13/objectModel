package bon.jo.user

import bon.jo.sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.domain.{User, UserAvatar, UserInfo}
import java.sql.PreparedStatement
import bon.jo.sql.Service
import bon.jo.sql.BaseSqlRequest
import bon.jo.sql.PSMapping
import bon.jo.sql.stmtSetObject
import bon.jo.sql.Join2TableRequest
import bon.jo.sql.Join2TableService
import bon.jo.sql.JoinType
import java.sql.Connection
import bon.jo.common.Convs
import bon.jo.domain.ImageInfo
import bon.jo.image.SqlServiceImage.given
import bon.jo.sql.SqlMappings.given
import Join2TableRequest.joinRequest
import bon.jo.sql.Alias
import bon.jo.image.ImageModel
object SqlServiceUser {
  

  given JoinType.Left[User,ImageInfo] = JoinType.Left()
  given Alias = Alias()
  
  trait UserWithImageService:
    self :  Join2TableService[User,ImageInfo,JoinType.Left] => 
      def findByName(name : String):Option[UserAvatar] = findBys(joinRequest.leftAlias+"."+UserModel.column.name ->name).map{
        (o) => UserAvatar(o._1,o._2)
      }
      def findById(id : Long):Option[UserAvatar] = findBys(joinRequest.leftAlias+"."+UserModel.column.id ->id).map{
        (o) => UserAvatar(o._1,o._2)
      }
  object JoinUserImageInfoRequest extends Join2TableRequest[User,ImageInfo,JoinType.Left]
  given Join2TableRequest[User,ImageInfo,JoinType.Left] = JoinUserImageInfoRequest
  object UserWithImageService:
    inline def apply()( using ()=> Connection) :UserWithImageService = 
      new Join2TableService[User,ImageInfo,JoinType.Left] with UserWithImageService:
        override lazy val joinCondition: String =
          s"${joinRequest.leftAlias}.${UserModel.column.avatarKey} = ${joinRequest.rightAlias}.${ImageModel.column.id}"
      
  
  
    
  type ServiceUser = Service[User,Long] with SqlServiceUser
  given BaseSqlRequest[User] = BaseSqlRequest[User](UserModel.userTable)
  given ResultSetMapping[User] with
    def apply(from : Int,r : ResultSet):User = 
      User(r.getLong(from),r.getString(from+1),r.getString(from+2), Option(r.getObject(from+3)).map(Convs.conv))
  given ResultSetMapping[UserInfo] with
    def apply(from : Int,r : ResultSet):UserInfo = 
      UserInfo(r.getLong(from),r.getString(from+1), None)

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

