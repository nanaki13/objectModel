package bon.jo.image

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import bon.jo.domain.{Image, ImageSend}
import ImageRepo.Command.*
import ImageRepo.Response.*
import bon.jo.sql.Sql
import bon.jo.image.SqlServiceImage.ServiceImage

import com.github.t3hnar.bcrypt._
object ImageRepo {


  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  AddImage(image: ImageSend, replyTo: ActorRef[Response]) extends Command
    case  GetImageById(id: Long, replyTo: ActorRef[Option[Image]]) extends Command
    case  ClearImages(id: Long,replyTo: ActorRef[Response]) extends Command
    case FindImages(name : String, replyTo: ActorRef[Option[Image]])

  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(images: ServiceImage): Behavior[Command] = Behaviors.receiveMessage {
    case AddImage(image, replyTo) =>
      if images.containsName(image.name) then
        replyTo ! KO("Image already exists")
        Behaviors.same  
      else
        images.create(Image(0,image.name,image.data))
        replyTo ! OK
        ImageRepo(images)
    case GetImageById(id, replyTo) =>
      replyTo ! images.readOption(id)
      Behaviors.same
    case ClearImages(id,replyTo) =>
      replyTo ! OK
      images.delete(id)
      Behaviors.same
    case FindImages(name,replyTo) =>
      replyTo ! images.find(name)
      Behaviors.same

  }

}