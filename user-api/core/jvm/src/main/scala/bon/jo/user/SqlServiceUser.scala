package bon.jo.user

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.domain.User
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.ConnectionTableService
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import java.sql.Connection
object SqlServiceUser {
  
  type ServiceUser = Service[User,Long] with SqlServiceUser
  
  given ResultSetMapping[User] with
    def apply(from : Int,r : ResultSet):User = 
      User(r.getLong(from).asInstanceOf,r.getObject(from+1).asInstanceOf,r.getObject(from+2).asInstanceOf)
  given ResultSetMapping[Long] with
    def apply(from : Int,r : ResultSet):Long = 
      r.getLong(from).asInstanceOf
  given PSMapping[User] with
     def apply(from : Int,v : User)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      stmtSetObject(from+2,v.pwd)
      from+3
  given PSMapping[Long] with
     def apply(from : Int,v : Long)(using PreparedStatement):Int=
      stmtSetObject(from,v)
      from+1
  inline def apply()( using ConnectionTableService[User]) : ServiceUser = new Service[User,Long] with SqlServiceUser
  inline def apply(c : ()=> Connection): ServiceUser=apply()(using ConnectionTableService[User](UserModel.userTable,c))

}
trait SqlServiceUser:
  self :  Service[User,Long] =>
    def find(name : String) :Option[User] = 
      self.findBy(UserModel.cName,name)

