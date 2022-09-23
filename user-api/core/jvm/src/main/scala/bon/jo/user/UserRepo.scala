package bon.jo.user

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import bon.jo.user.UserModel.User
import UserRepo.Command.*
import UserRepo.Response.*
import bon.jo.sql.Sql
import bon.jo.user.SqlServiceUser.ServiceUser
import bon.jo.user.UserModel.UserLogin
object UserRepo {


  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  AddUser(user: UserLogin, replyTo: ActorRef[Response]) extends Command
    case  GetUserById(id: Long, replyTo: ActorRef[Option[User]]) extends Command
    case  ClearUsers(id: Long,replyTo: ActorRef[Response]) extends Command
    case FindUsers(name : String, replyTo: ActorRef[Option[User]])

  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(users: ServiceUser,id : Long): Behavior[Command] = Behaviors.receiveMessage {
   /* case AddUser(user, replyTo) if users.contains(user.id) =>
      replyTo ! KO("User already exists")
      Behaviors.same */
    case AddUser(user, replyTo) =>
      replyTo ! OK
      users.create(User(id,user.name,user.pwd))
      UserRepo(users,id+1)
    case GetUserById(id, replyTo) =>
      replyTo ! users.readOption(id)
      Behaviors.same
    case ClearUsers(id,replyTo) =>
      replyTo ! OK
      users.delete(id)
      Behaviors.same
    case FindUsers(name,replyTo) =>
      replyTo ! users.find(name)
      Behaviors.same

  }

}