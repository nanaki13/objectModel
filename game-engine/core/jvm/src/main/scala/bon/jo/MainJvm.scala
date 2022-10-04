package bon.jo
import bon.jo.pong.*
import akka.actor.typed.ActorSystem
import bon.jo.server.Server
import bon.jo.server.Server.RouteMaker
import java.sql.DriverManager
import bon.jo.server.Message
import java.sql.Connection
import bon.jo.route.ScoreRoutes
import bon.jo.service.ScoreRepo
import bon.jo.service.SqlServiceScore
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.ActorRef
import bon.jo.user.TokenRepo
import bon.jo.model.ScoreModel
import bon.jo.sql.Sql.{stmtDo, stmt}
import scala.util.Try
object MainJvm extends Server:
//libraryDependencies += "org.postgresql" % "postgresql" % "42.5.0"
 // Class.forName("org.sqlite.JDBC")
  //val monoCon = DriverManager.getConnection("jdbc:sqlite:sample2.db")
  val monoCon = DriverManager.getConnection("jdbc:postgresql://db/postgres","postgres","docker")
  println(monoCon.getMetaData().getCatalogSeparator())
  println(monoCon.getMetaData().getIdentifierQuoteString())
  given con : (() => Connection) = () => monoCon
 /* stmtDo() {
    stmt.execute("DROP TABLE if exists score; ")
  }*/
  def p[T](e: T): T =
    println(e)
    e
  override def init(): Unit = 
    super.init()
    given  Connection = con()
    Try{
          stmtDo() {
        ScoreModel.scoreTable.createSql
          .split(";")
          .map(p)
          .map(stmt.executeUpdate)
          .foreach(println)
    
        }
    }

  inline def launch(): Unit =
    init()
    def route : RouteMaker = ctx =>
      given ActorSystem[_] = ctx.system
      Some(
        ScoreRoutes(
          ctx.spawn(ScoreRepo(SqlServiceScore()), "ScoreRepo")
        ).route
      )

    val system: ActorSystem[Message] =
      ActorSystem(MainJvm("localhost", 8080, route), "GameServer")

@main
def launch(): Unit =
  MainJvm.launch()
