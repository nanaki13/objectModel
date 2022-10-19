package bon.jo
import bon.jo.pong.*
import akka.actor.typed.ActorSystem
import bon.jo.server.Server
import bon.jo.server.Server.RouteMaker
import java.sql.DriverManager
import bon.jo.server.Message
import java.sql.Connection
import bon.jo.route.ScoreRoutes
import bon.jo.route.PostRoutes
import bon.jo.service.ScoreRepo
import bon.jo.service.SqlServiceScore
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.ActorRef
import bon.jo.user.TokenRepo
import bon.jo.model.ScoreModel
import bon.jo.sql.Sql.{stmtDo, stmt}
import scala.util.Try
import bon.jo.route.StaticRoute
import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import bon.jo.model.PostModel
import scala.util.Failure
import scala.util.Success
import scala.concurrent.ExecutionContext
import bon.jo.service.PostRepo
import bon.jo.service.SqlServicePost
import concurrent.duration.DurationInt
object MainJvm extends Server:

  extension (key: String)
    def readEnv(): Option[String] = scala.sys.env.get(key)
    def readEnvOrElse(defaultValue: => String): String =
      readEnv().getOrElse(defaultValue)
  val monoCon = (for {
    url <- "BON_JO_GAME_ENGINE_DB_URL_JDBC".readEnv()
    user <- "BON_JO_GAME_ENGINE_DB_USER".readEnv()
    pass <- "BON_JO_GAME_ENGINE_DB_PASSWORD".readEnv()
  } yield DriverManager.getConnection(url, user, pass))
    .getOrElse(DriverManager.getConnection("jdbc:sqlite:sample2.db"))
  

  println(monoCon.getMetaData().getCatalogSeparator())
  println(monoCon.getMetaData().getIdentifierQuoteString())
  given con: (() => Connection) = () => monoCon
  /* stmtDo() {
    stmt.execute("DROP TABLE if exists score; ")
  }*/
  def p[T](e: T): T =
    println(e)
    e
  override def init(): Unit =
    // super.init()
    given Connection = con()
    
    Try {
      stmtDo() {
        ScoreModel.table.createSql
          .split(";")
          .map(p)
          .map(stmt.executeUpdate)
          .foreach(println)

      }
    } match
      case Failure(exception) => println(exception)
      case Success(value)     =>
    Try {
      stmtDo() {
        PostModel.table.createSql
          .split(";")
          .map(p)
          .map(stmt.executeUpdate)
          .foreach(println)

      }
    } match
      case Failure(exception) => println(exception)
      case Success(value)     =>

  def launch(): Unit =
    super.init()
    init()
    def route: RouteMaker = ctx =>
      given ActorSystem[_] = ctx.system
      given ExecutionContext  = ctx.system.executionContext
      given Timeout = 3.seconds
      Some(
        concat(
          ScoreRoutes(
            ctx.spawn(ScoreRepo(SqlServiceScore()), "ScoreRepo")
          ).route,
          PostRoutes(
            ctx.spawn(PostRepo(SqlServicePost()), "PostRepo")
          ).route,
          StaticRoute()
        )
      )

    val system: ActorSystem[Message] =
      ActorSystem(MainJvm("0.0.0.0", 8080, route), "GameServer")

@main
def launch(): Unit =
  MainJvm.launch()
