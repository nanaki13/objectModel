package bon.jo.sql
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import java.time.LocalDate
import java.sql.DriverManager

import bon.jo.sql.stmtSetObject
import bon.jo.sql.{ResultSetMapping, BaseSqlRequest, PSMapping}
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

import scala.util.Success
import scala.util.Failure
import scala.util.Try
import bon.jo.model.ScoreModel
import bon.jo.service.SqlServiceScore
import bon.jo.model.ScoreModel.Score
import bon.jo.model.ScoreModel.toDomain
import java.time.LocalDateTime
import bon.jo.user.UserModel
import bon.jo.image.ImageModel
import bon.jo.user.SqlServiceUser.apply
import bon.jo.user.SqlServiceUser
import bon.jo.domain.User

class AllTest extends AnyFlatSpec with should.Matchers:
  Class.forName("org.sqlite.JDBC")

  given con: Connection = DriverManager.getConnection("jdbc:sqlite::memory:")
  given conn:( () => Connection) = () => con

  def p[T](e: T): T =
    println(e)
    e
  stmtDo() {
    ScoreModel.table.createSql
      .split(";")
      .map(p)
      .map(stmt.executeUpdate)
      .foreach(println)
    UserModel.userTable.createSql
      .split(";")
      .map(p)
      .map(stmt.executeUpdate)
      .foreach(println)
  
    ImageModel.imageTable.createSql
      .split(";")
      .map(p)
      .map(stmt.executeUpdate)
      .foreach(println)
  }

  "A service" should "read,upate delete insert" in {

    val service = SqlServiceScore()
    val userService = SqlServiceUser()
    userService.create(User(1,"test1","",None))
    userService.create(User(2,"test2","",None))
    userService.create(User(3,"test3","",None))
    val score = service.create(Score(idGame  = 1,lvl= 1,idUser =  1, LocalDateTime.now(), 1200))
    val score2 = service.create(Score(idGame  = 1,lvl= 1,idUser =  2, LocalDateTime.now(), 1300))
    val score3 = service.create(Score(idGame  = 1,lvl= 1,idUser =  3, LocalDateTime.now(), 1400))
    println(score)
    service.read((1, 1,1)) should be(score)
    println(service.readScore(1,1).map(_.score).toSet should be(Set(score,score2,score3).map(_.toDomain())))
    con.close
  }
