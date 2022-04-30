package bon.jo.server

import bon.jo.user.SqlServiceUser
import akka.actor.typed.PostStop
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.Http

import scala.util.{ Success, Failure }
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.server.Directives._
import java.sql.Connection
import java.sql.DriverManager
import org.json4s.DefaultFormats
import org.json4s.Formats
import scala.concurrent.Future
import akka.actor.typed.ActorSystem
import bon.jo.user.UserRepo
import bon.jo.user.UserModel
import bon.jo.sql.Sql.doSql
import bon.jo.sql.Sql.executeUpdate
import bon.jo.user.UserModel.User
object Server {

  Class.forName("org.sqlite.JDBC")
  given con : Connection = DriverManager.getConnection("jdbc:sqlite:sample2.db")
  println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"+UserModel.userTable.createSql)
  /*doSql("DROP TABLE if exists user "){
    executeUpdate()  
  }*/
  

  val service = SqlServiceUser(()=>con)
  try 
    
    doSql(UserModel.userTable.createSql){
      executeUpdate()  
    }
  catch
    case _ => 
  try 
    
    service.create(User(1,"nanaki","test"))
  catch
    case _ => 
  sealed trait Message
  private final case class StartFailed(cause: Throwable) extends Message
  private final case class Started(binding: ServerBinding) extends Message
  case object Stop extends Message

  def apply(host: String, port: Int): Behavior[Message] = Behaviors.setup { ctx =>

    given ActorSystem[_] = ctx.system

    val buildJobRepository = ctx.spawn(UserRepo(service), "UserRepository")

    given Formats = DefaultFormats
    val routes = new UserRoutes(buildJobRepository)
    val routesAuth = new AuthRoutes(buildJobRepository)
    val serverBinding: Future[Http.ServerBinding] =
      Http().newServerAt(host, port).bind(concat(routes.theUserRoutes,routesAuth.theAuthRoutes))
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
}

@main
def launch(): Unit = {
  val system: ActorSystem[Server.Message] =
    ActorSystem(Server("localhost", 8080), "BuildUsersServer")
}