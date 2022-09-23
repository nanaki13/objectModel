package bon.jo.user

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import bon.jo.user.UserModel.UserInfo
import TokenRepo.Command.*
import TokenRepo.Response.*
import bon.jo.sql.Sql
import bon.jo.user.SqlServiceUser.ServiceUser
import pdi.jwt.{Jwt, JwtClaim}
import org.json4s.native.Serialization
import org.json4s.Formats
import org.json4s.DefaultFormats
import pdi.jwt.JwtAlgorithm
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
object TokenRepo {

  val key = scala.sys.env.getOrElse("bon.jo.secret","secret")
    // key: String = "secretKey"
  val algo = JwtAlgorithm.HS256
  given Formats = DefaultFormats

  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  GetToken(user : UserInfo, replyTo: ActorRef[String]) extends Command


  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(): Behavior[Command] = Behaviors.receiveMessage {
    case GetToken(user, replyTo) =>
      val jwtClaim = JwtClaim.apply(content = Serialization.write(user),expiration = Some(ZonedDateTime.now().plusSeconds(30).toEpochSecond()))
      replyTo ! Jwt.encode(jwtClaim,key,algo)
      Behaviors.same

  }

}