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
import scala.util.Try
import bon.jo.image.ImageModel
import bon.jo.image.ImageRepo
import bon.jo.image.SqlServiceImage
import scala.concurrent.ExecutionContext
import concurrent.duration.DurationInt
import akka.util.Timeout


enum Message:
  case StartFailed(cause: Throwable)
  case Started(binding: ServerBinding)
  case Stop  
object Server extends Server{

  Class.forName("org.sqlite.JDBC")
  //val dir = sys.env.get("HOME").orElse(sys.env.get("USERPROFILE")).get 
  val monoCon = DriverManager.getConnection(s"jdbc:sqlite:sample2.db")
  given con : (() => Connection) = () => monoCon
 
}

trait Server:
  given Formats = JsonSupport.format
  given con : (() => Connection)
  
  lazy val service = SqlServiceUser()
  val imageSerice = SqlServiceImage()
  def init():Unit = 
    given Connection = con()
    import bon.jo.sql.DBType.given

   
    /* doSql(s"DROP TABLE if exists ${UserModel.userTable.name} "){
      execute()  
    }*/
    def p[T](e : T):T =
      println(e)
      e
    try 
      stmtDo(){
        UserModel.userTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
      }
    catch
      case e => println(e.getMessage())
    try 
      stmtDo(){
        ImageModel.imageTable.createSql.split(";").map(p).map(stmt.executeUpdate).foreach(println)
      }
    catch
      case e => println(e.getMessage())
  
  type RouteMaker =    ActorRef[TokenRepo.Command] ?=>  ActorContext[Message] => Option[Route]
  def apply(host: String, port: Int,route : RouteMaker = ctx => None): Behavior[Message] = Behaviors.setup { ctx =>

    given ActorSystem[_] = ctx.system
    val initId = Try{
      service.maxId()+1l
    }.recover(_ => 1l).get
    given buildJobRepository : ActorRef[UserRepo.Command]  = ctx.spawn(UserRepo(service,initId), "UserRepository")
    given  tokenRepo : ActorRef[TokenRepo.Command] = ctx.spawn(TokenRepo(), "TokenRepository")
    given  imageRepo : ActorRef[ImageRepo.Command] = ctx.spawn(ImageRepo(imageSerice), "ImageRepository")
   
    given ExecutionContext = ctx.system.executionContext
    given Timeout = 3.seconds
    
    val routes = new UserRoutes(buildJobRepository)
    val tokenRoute = new TokenRoutes()
    val imgRoute = new ImageRoutes(imageRepo)
    val base = concat(routes.theUserRoutes,tokenRoute.theTokenRoutes,tokenRoute.theTokenRoutes,imgRoute.theImageRoutes)
    val allRoute = 
      route(ctx) match
        case Some(r) => concat(base,r)
        case _ => base
      
    val serverBinding: Future[Http.ServerBinding] =
      Http().newServerAt(host, port).enableHttps(HttpsConf()).bind(allRoute)
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
    val servuer = Server("localhost", 8080)
    Server.init()
    ActorSystem(servuer, "BuildUsersServer")
}