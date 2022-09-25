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
import bon.jo.user.ScoreModel
class AllTest extends AnyFlatSpec with should.Matchers {
  Class.forName("org.sqlite.JDBC")
  given con : Connection = DriverManager.getConnection("jdbc:sqlite:sample2.db")

 
 
  stmtDo(){
    stmt.execute("DROP TABLE if exists score; ")
  }
  def p[T](e : T):T =
    println(e)
    e
  stmtDo(){
     ScoreModel.scoreTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
  }



   "A service" should "read,upate delete insert" in {

    
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
