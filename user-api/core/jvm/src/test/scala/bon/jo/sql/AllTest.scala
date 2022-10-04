package bon.jo.sql
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import java.time.LocalDate
import java.sql.DriverManager


import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.{ResultSetMapping,BaseSqlRequest,PSMapping}
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
import bon.jo.user.SqlServiceUser
import bon.jo.domain.User
import bon.jo.sql.Sql.JoinBaseSqlRequest
import bon.jo.sql.Sql.Table
import bon.jo.sql.Sql.Column
import bon.jo.sql.Sql.Alias
import bon.jo.sql.Sql.JoinService
class AllTest extends AnyFlatSpec with should.Matchers {
  Class.forName("org.sqlite.JDBC")

  val monoCon = DriverManager.getConnection("jdbc:sqlite:test.db")
  given con : (() => Connection) = () => monoCon
  given c: Connection = con()

 
 
  stmtDo(){
    stmt.execute("DROP TABLE if exists users; ")
  }
  stmtDo(){
    stmt.execute("DROP TABLE if exists score; ")
  }
  def p[T](e : T):T =
    println(e)
    e
  stmtDo(){
     UserModel.userTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
  }



 


  
  val service =SqlServiceUser()

   "A service" should "read,upate delete insert" in {
    service.create(User(1,"tett","pass")) 
    println(service.read(1))
  
    println(service.update(1,User(1,"toto","pass")))
    val ex = intercept[SQLException](service.create(User(2,"toto","pass"))) 
    
    println(service.read(1))
    //println(service.delete(1))
    service.readOption(1) should be (Some(User(1,"toto","pass")))
    service.findBy("name","toto") should be (Some(User(1,"toto","pass")))
    println(service.delete(1))
    service.readOption(1) should be (None)
    import bon.jo.sql.Sql.Table.*
    import bon.jo.sql.Sql.Column.*
    val scoreTable = Table{
      tableName("score")
        Column{columnName("id_user");_type("BIGINT");id} 
        Column{columnName("score");_type("INT")} 
    }
     stmtDo(){
      scoreTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
    }
    import bon.jo.user.SqlServiceUser.given
    case class Score(idUser : Long,score:Int)
    given BaseSqlRequest[Score] = BaseSqlRequest[Score](scoreTable)
    given BaseSqlRequest[User] = service.request
    given Alias = Alias()
    given PSMapping[Score] with
     def apply(from : Int,v : Score)(using PreparedStatement):Int=
      stmtSetObject(from,v.idUser)
      stmtSetObject(from+1,v.score)
      from+2
    object userJoinScore extends JoinBaseSqlRequest[User,Score]
    object scoreService extends Service[Score,Long]
    given JoinBaseSqlRequest[User,Score] = userJoinScore

    
    given ResultSetMapping[Score] with
      def apply(from : Int,r : ResultSet):Score = 
        Score(r.getLong(from).asInstanceOf,r.getObject(from+1).asInstanceOf)
    
    object joinUserScore extends JoinService[User,Score]:
      import JoinBaseSqlRequest.joinRequest
      override lazy val joinCondition: String = joinRequest.leftAlias+".id = "+ joinRequest.rightAlias+".id_user"

    service.create(User(1,"tett","pass")) 
    scoreService.create(Score(1,100))
    println(joinUserScore.findBys("id_user"-> 1))
    c.close  
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
