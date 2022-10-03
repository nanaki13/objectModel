package bon.jo.server

import bon.jo.user.SqlServiceUser
import akka.actor.typed.PostStop
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.Http

import scala.util.{ Success, Failure }
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.ActorContext
import akka.http.scaladsl.server.Route

import akka.http.scaladsl.server.Directives._
import java.sql.Connection
import java.sql.DriverManager
import org.json4s.Formats
import scala.concurrent.Future
import akka.actor.typed.ActorSystem
import bon.jo.user.UserRepo
import bon.jo.user.UserModel
import bon.jo.sql.Sql.{doSql, stmtDo, stmt}
import bon.jo.sql.Sql.execute
import bon.jo.domain.User
import bon.jo.user.TokenRepo
import akka.actor.typed.ActorRef
import Message.*


enum Message:
  case StartFailed(cause: Throwable)
  case Started(binding: ServerBinding)
  case Stop  
object Server extends Server{

  Class.forName("org.sqlite.JDBC")
  val monoCon = DriverManager.getConnection("jdbc:sqlite:sample2.db")
  given con : (() => Connection) = () => monoCon
 
}

trait Server:
  given Formats = JsonSupport.format
  given con : (() => Connection)
  
  val service = SqlServiceUser()
  def init():Unit = 
    given Connection = con()
    println(UserModel.userTable.createSql)
   
    doSql(s"DROP TABLE if exists ${UserModel.userTable.name} "){
      execute()  
    }
    def p[T](e : T):T =
      println(e)
      e
    try 
      stmtDo(){
        UserModel.userTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
      }
    catch
      case _ => 
  
  type RouteMaker =    ActorRef[TokenRepo.Command] ?=>  ActorContext[Message] => Option[Route]
  def apply(host: String, port: Int,route : RouteMaker = ctx => None): Behavior[Message] = Behaviors.setup { ctx =>

    given ActorSystem[_] = ctx.system

    val buildJobRepository = ctx.spawn(UserRepo(service,service.maxId()+1), "UserRepository")
    given  tokenRepo : ActorRef[TokenRepo.Command] = ctx.spawn(TokenRepo(), "TokenRepository")
   
    
    val routes = new UserRoutes(buildJobRepository)
    val tokenRoute = new TokenRoutes(buildJobRepository,tokenRepo)
    val allRoute = 
      route(ctx) match
        case Some(r) => concat(routes.theUserRoutes,tokenRoute.theTokenRoutes,r)
        case _ => concat(routes.theUserRoutes,tokenRoute.theTokenRoutes)
      
    val serverBinding: Future[Http.ServerBinding] =
      Http().newServerAt(host, port).bind(allRoute)
    ctx.pipeToSelf(serverBinding) {
      case Success(binding) => Started(binding)
      case Failure(ex)      => StartFailed(ex)
    }

    def running(binding: ServerBinding): Behavior[Message] =
      Behaviors.receiveMessagePartial[Message] {
        case Stop =>
          println( "Server online ")
          ctx.log.info(
            "Stopping server http://{}:{}/",
            binding.localAddress.getHostString,
            binding.localAddress.getPort)
          Behaviors.stopped
      }.receiveSignal {
        case (_, PostStop) =>
          binding.unbind()
          Behaviors.same
      }

    def starting(wasStopped: Boolean): Behaviors.Receive[Message] =
      Behaviors.receiveMessage[Message] {
        case StartFailed(cause) =>
          throw new RuntimeException("Server failed to start", cause)
        case Started(binding) =>
          println( "Server online ")
          ctx.log.info(
            "Server online at http://{}:{}/",
            binding.localAddress.getHostString,
            binding.localAddress.getPort)
          if (wasStopped) ctx.self ! Stop
          running(binding)
        case Stop =>
          // we got a stop message but haven't completed starting yet,
          // we cannot stop until starting has completed
          starting(wasStopped = true)
      }

    starting(wasStopped = false)
  }
@main
def launch(): Unit = {
  val system: ActorSystem[Message] =
    ActorSystem(Server("localhost", 8080), "BuildUsersServer")
}