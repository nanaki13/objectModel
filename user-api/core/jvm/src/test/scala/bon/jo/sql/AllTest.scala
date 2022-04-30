package bon.jo.sql
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import java.time.LocalDate
import java.sql.DriverManager


import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.{ResultSetMapping,ConnectionTableService,PSMapping}
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.executeUpdate
import bon.jo.sql.Sql.doSql
import java.sql.ResultSet
import java.sql.PreparedStatement
import java.sql.Connection
import bon.jo.user.UserModel
class AllTest extends AnyFlatSpec with should.Matchers {
  Class.forName("org.sqlite.JDBC")
  given con : Connection = DriverManager.getConnection("jdbc:sqlite:sample2.db")

 
  println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"+UserModel.userTable.createSql)
   doSql("DROP TABLE if exists user "){
    executeUpdate()  
  }
  doSql(UserModel.userTable.createSql){
    executeUpdate()  
  }



 
  case class User(id : Long,name : String)

  given ResultSetMapping[User] with
    def apply(r : ResultSet):User = 
      User(r.getLong(1).asInstanceOf,r.getObject(2).asInstanceOf)
  given PSMapping[User] with
     def apply(from : Int,v : User)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      println(summon[PreparedStatement])
      from+2
  given PSMapping[Int] with
     def apply(from : Int,v : Int)(using PreparedStatement):Int=
      stmtSetObject(from,v)
      from+1
  given ConnectionTableService[User] = ConnectionTableService(UserModel.userTable,()=>con)

  val service = Service[User,Int]

  "A service" should "read,upate delete insert" in {
    service.create(User(1,"tett")) 
    println(service.read(1))

    println(service.update(1,User(1,"toto")))
    println(service.read(1))
   // println(service.delete(1))
    service.readOption(1) should be (None)
    con.close  
  }


  

}
