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
import bon.jo.sql.Sql.execute
import bon.jo.sql.Sql.doSql
import bon.jo.sql.Sql.stmtDo
import bon.jo.sql.Sql.stmt
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement
import java.sql.Connection
import bon.jo.user.UserModel
import scala.util.Success
import scala.util.Failure
import scala.util.Try
class AllTest extends AnyFlatSpec with should.Matchers {
  Class.forName("org.sqlite.JDBC")
  given con : Connection = DriverManager.getConnection("jdbc:sqlite:sample2.db")

 
 
  stmtDo(){
    stmt.execute("DROP TABLE if exists user; ")
  }
  def p[T](e : T):T =
    println(e)
    e
  stmtDo(){
     UserModel.userTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
  }



 
  case class User(id : Long,name : String)

  given ResultSetMapping[User] = (from,r )=> 
      User(r.getLong(from),r.getString(from+1))
  given PSMapping[User] with
     def apply(from : Int,v : User)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      println(summon[PreparedStatement])
      from+3
  given PSMapping[Int] with
     def apply(from : Int,v : Int)(using PreparedStatement):Int=
      stmtSetObject(from,v)
      from+1
  given ConnectionTableService[User] = ConnectionTableService(UserModel.userTable,()=>con)
  given ResultSetMapping[Int] = (from,r) => r.getInt(from)
  val service = Service[User,Int]

   "A service" should "read,upate delete insert" in {
    service.create(User(1,"tett")) 
    println(service.read(1))
  
    println(service.update(1,User(1,"toto")))
    val ex = intercept[SQLException](service.create(User(2,"toto")) ) 
    
    println(service.read(1))
    //println(service.delete(1))
    service.readOption(1) should be (Some(User(1,"toto")))
    service.findBy("name","toto") should be (Some(User(1,"toto")))
    println(service.delete(1))
    service.readOption(1) should be (None)
    
    con.close  
   }

  /*"A service" should "read,upate delete insert" in {
    service.create(User(1,"tett")) 
    println(service.read(1))

    println(service.update(1,User(1,"toto")))
   /* val ex= intercept[Exception] {
       service.create(User(2,"toto"))
    }
    ex.getMessage() should be ("") */
    println(service.read(1))
    println(service.delete(1))
  //  service.readOption(1) should be (None)
    
    con.close  
  }*/
  

}
