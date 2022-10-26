package bon.jo.sql
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import java.time.LocalDate
import java.sql.DriverManager


import bon.jo.sql.stmtSetObject
import bon.jo.sql.{ResultSetMapping,BaseSqlRequest,PSMapping}
import bon.jo.sql.Service
import bon.jo.sql.executeUpdate
import bon.jo.sql.execute
import bon.jo.sql.doSql
import bon.jo.sql.stmtDo
import bon.jo.sql.stmt
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.PreparedStatement
import java.sql.Connection
import bon.jo.user.UserModel
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import bon.jo.user.SqlServiceUser
import bon.jo.user.SqlServiceUser.UserWithImageService
import bon.jo.domain.User
import bon.jo.sql.Join2TableRequest
import bon.jo.sql.Table
import bon.jo.sql.Column 
import bon.jo.sql.Alias
import bon.jo.sql.Join2TableService
import bon.jo.sql.Join3TableService
import bon.jo.sql.JoinType
import bon.jo.sql.JoinDef
import bon.jo.domain.ImageInfo
import bon.jo.domain.UserInfo
import bon.jo.image.ImageModel
import bon.jo.sql.Join3TableRequest
import java.time.LocalDateTime
import java.nio.file.Files
import java.nio.file.Path
class AllTest extends AnyFlatSpec with should.Matchers {
  Class.forName("org.sqlite.JDBC")

  Files.delete(Path.of("test.db"))
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
    service.create(User(1,"tett","pass",None)) 
    println(service.read(1))
  
    println(service.update(1,User(1,"toto","pass",None)))
    val ex = intercept[SQLException](service.create(User(2,"toto","pass",None))) 
    
    println(service.read(1))
    //println(service.delete(1))
    service.readOption(1) should be (Some(User(1,"toto","pass",None)))
    service.findBy("name","toto") should be (Some(User(1,"toto","pass",None)))
    println(service.delete(1))
    service.readOption(1) should be (None)
    import bon.jo.sql.Table.*
    import bon.jo.sql.Column .*
    val scoreTable = Table{
      tableName("score")
        Column{columnName("id_user");_type("BIGINT");id} 
        Column{columnName("score");_type("INT")} 
    }
    stmtDo(){
      scoreTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
    }
    stmtDo(){
      ImageModel.imageTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
    }
    import bon.jo.user.SqlServiceUser.given
    case class Score(idUser : Long,score:Int)
    given BaseSqlRequest[Score] = BaseSqlRequest[Score](scoreTable)
    given BaseSqlRequest[User] = service.request
    given BaseSqlRequest[UserInfo] = BaseSqlRequest[UserInfo](UserModel.userInfoTable)
    import bon.jo.image.SqlServiceImage.given
    import bon.jo.sql.SqlMappings.given
    given Alias = Alias()
    given PSMapping[Score] with
     def apply(from : Int,v : Score)(using PreparedStatement):Int=
      stmtSetObject(from,v.idUser)
      stmtSetObject(from+1,v.score)
      from+2
    given JoinDef[Score,UserInfo] = JoinDef(JoinType.Default(), (l,r) => s"$l.${"id_user"} = $r.${UserModel.column.id} ") 
    given JoinDef[UserInfo,ImageInfo] = JoinDef(JoinType.Left(), (l,r) => s"$l.${UserModel.column.avatarKey} = $r.${ImageModel.column.id} ") 
    given ResultSetMapping[Score] = (from,y)=> 
      Score(y.getLong(from),y.getInt(from+1))
    given joinRequest: Join3TableRequest[Score, UserInfo,ImageInfo,JoinType.Default,JoinType.Left] =
      new Join3TableRequest[Score, UserInfo,ImageInfo,JoinType.Default,JoinType.Left]() {}

    val joinService = Join3TableService[Score, UserInfo,ImageInfo,JoinType.Default,JoinType.Left]()
    val scoreService = Service[Score,Long]
    service.create(User(1,"tett","pass",None)) 
    scoreService.create(Score(1,100))
    println(joinService.findBys("id_user"-> 1))
    val uiService = UserWithImageService()
    println(uiService.findById(1))
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
