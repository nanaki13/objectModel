package bon.jo.user

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.domain.User
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.BaseSqlRequest
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import java.sql.Connection
object SqlServiceUser {
  
  type ServiceUser = Service[User,Long] with SqlServiceUser
  given BaseSqlRequest[User] = BaseSqlRequest[User](UserModel.userTable)
  given ResultSetMapping[User] with
    def apply(from : Int,r : ResultSet):User = 
      User(r.getLong(from),r.getString(from+1),r.getString(from+2),Option(r.getLong(from+3)))
  given ResultSetMapping[Long] with
    def apply(from : Int,r : ResultSet):Long = 
      r.getLong(from).asInstanceOf
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
  given PSMapping[Long] with
     def apply(from : Int,v : Long)(using PreparedStatement):Int=
      stmtSetObject(from,v)
      from+1
  inline def apply()( using ()=> Connection) : ServiceUser = new Service[User,Long] with SqlServiceUser
  

}
trait SqlServiceUser:
  self :  Service[User,Long] =>
    def find(name : String) :Option[User] = 
      self.findBy(UserModel.column.name,name)
    def containsName(name : String): Boolean = 
      self.contains(UserModel.column.name -> name)

