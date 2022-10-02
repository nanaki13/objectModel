package bon.jo.sql
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import java.time.LocalDate
import java.sql.DriverManager

import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.{ResultSetMapping, BaseSqlRequest, PSMapping}
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

import scala.util.Success
import scala.util.Failure
import scala.util.Try
import bon.jo.model.ScoreModel
import bon.jo.service.SqlServiceScore
import bon.jo.domain.Score
import java.time.LocalDateTime
class AllTest extends AnyFlatSpec with should.Matchers:
  Class.forName("org.sqlite.JDBC")
  given con: Connection = DriverManager.getConnection("jdbc:sqlite:sample2.db")
  given conn:( () => Connection) = () => con
  stmtDo() {
    stmt.execute("DROP TABLE if exists score; ")
  }
  def p[T](e: T): T =
    println(e)
    e
  stmtDo() {
    ScoreModel.scoreTable.createSql
      .split(";")
      .map(p)
      .map(stmt.executeUpdate)
      .foreach(println)
  }

  "A service" should "read,upate delete insert" in {

    val service = SqlServiceScore()
    val score = service.create(Score(idGame  = 1,lvl= 1,idUser =  1, LocalDateTime.now(), 1200))
    val score2 = service.create(Score(idGame  = 1,lvl= 1,idUser =  2, LocalDateTime.now(), 1300))
    val score3 = service.create(Score(idGame  = 1,lvl= 1,idUser =  3, LocalDateTime.now(), 1400))
    println(score)
    service.read((1, 1,1)) should be(score)
    println(service.readScore(1,1).toSet should be(Set(score,score2,score3)))
    con.close
  }
