package bon.jo.user

import akka.actor.typed.{ ActorRef, Behavior,ActorSystem }
import akka.actor.typed.scaladsl.Behaviors
import bon.jo.domain.UserInfo
import TokenRepo.Command.*
import TokenRepo.Response.*
import bon.jo.sql.Sql
import bon.jo.user.SqlServiceUser.ServiceUser
import pdi.jwt.{Jwt, JwtClaim}
import org.json4s.native.Serialization
import org.json4s.Formats
import pdi.jwt.JwtAlgorithm
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.util.Try
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
import bon.jo.server.JsonSupport
import scala.concurrent.Future
import akka.util.Timeout

import bon.jo.domain.User
import scala.concurrent.ExecutionContext
object TokenRepo {

  val key = scala.sys.env.getOrElse("bon.jo.secret","secret")
    // key: String = "secretKey"
  val algo = JwtAlgorithm.HS256
  given Formats = JsonSupport.format

  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  GetToken(user : UserInfo, replyTo: ActorRef[String]) extends Command
    case  ParseToken(token : String, replyTo: ActorRef[Try[JwtClaim]]) extends Command


  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(): Behavior[Command] = Behaviors.receiveMessage {
    case GetToken(user, replyTo) =>

      val jwtClaim = JwtClaim.apply(content = Serialization.write(user),expiration = Some(ZonedDateTime.now().plusDays(7).toEpochSecond()))
      replyTo ! Jwt.encode(jwtClaim,key,algo)
      Behaviors.same

    case ParseToken(token,replyTo) => 
      replyTo ! Jwt.decode(token,key,Seq(algo))
      Behaviors.same

  }

  extension (c : JwtClaim)
    def toUserInfo : UserInfo = Serialization.read(c.content)
    def userFromDb(using users : ActorRef[UserRepo.Command]): (Timeout,ActorSystem[_],ExecutionContext) ?=> Future[User] = 
      val ui = toUserInfo
      users.ask[Option[User]](UserRepo.Command.GetUserById(ui.id,_ )).map(_.getOrElse(throw new IllegalStateException(s"no user ${ui.id}")))

}