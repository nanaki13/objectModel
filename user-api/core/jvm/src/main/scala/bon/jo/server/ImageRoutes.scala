package bon.jo.server

import akka.actor.typed.ActorSystem
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.MissingFormFieldRejection
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.Multipart
import akka.http.scaladsl.model.Multipart.BodyPart
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.actor.typed.ActorRef
import bon.jo.image.ImageRepo
import bon.jo.domain.Image
import bon.jo.image.ImageRepo.Command
import bon.jo.image.ImageRepo.Response
import org.json4s.Formats
import scala.concurrent.ExecutionContext
import bon.jo.domain.ImageSend
import bon.jo.user.TokenRepo
import bon.jo.user.TokenRepo.{toUserInfo as toUi}
import scala.collection.mutable.ArrayBuffer
import scala.util.Success
import scala.util.Failure
import akka.http.scaladsl.model.HttpEntity.apply
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ContentType
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.server.Rejection
import akka.stream.Materializer
object ImageRoutes:
    def extractData(fieldName : String):(Materializer,ExecutionContext) ?=> Directive1[Array[Byte]] =
      entity(as[Multipart.FormData]).flatMap { formData =>

        // collect all parts of the multipart as it arrives into a map
        val allPartsF: Future[Array[Byte]] = formData.parts
          .mapAsync[Array[Byte]](1) {

            case b: BodyPart if b.name == fieldName =>
              b.entity.dataBytes
                .runFold(ArrayBuffer[Byte]())((buff, data) => buff.addAll(data))
                .map(_.toArray)

            case b: BodyPart =>
              b.entity.discardBytes()
              Future.successful(Array.emptyByteArray)

          }
          .runFold(ArrayBuffer[Byte]())((map, tuple) => map.addAll(tuple))
          .map(_.toArray)

        onSuccess(allPartsF)

      }
class ImageRoutes(buildImageRepository: ActorRef[ImageRepo.Command])(using
    ActorRef[TokenRepo.Command],
    ActorSystem[_],
    Manifest[Image],
    Manifest[Seq[Image]],
    Formats
) extends JsonSupport[Image]
    with CORSHandler
    with TokenRouteGuard {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  given ec: ExecutionContext = summon[ActorSystem[_]].executionContext
  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException

  given t: Timeout = 3.seconds


  def findExtension(s : String):String = 
    val dotindex = s.lastIndexOf('.')
    if dotindex != -1 then
      s.substring(dotindex+1)
    else ""
  lazy val theImageRoutes: Route =
    corsHandler {
      pathPrefix("images") {
        concat(
         (get & path( Segment )){ name =>

             rejectEmptyResponse {
                    val maybeImage: Future[Option[Image]] = buildImageRepository.ask(Command.FindImages(name, _))
                    val ct : ContentType  = findExtension(name).toLowerCase match
                      case "jpg" => ContentType(MediaTypes.`image/jpeg`)
                      case ext => throw new IllegalStateException(s"$ext not supported")
                    val httpEntity =  for{
                      opImg <- maybeImage
                      img = for i <- opImg yield (HttpEntity(i.data))
                    } yield img
                  
                    complete(httpEntity)
          } 
            
            
            
          }
        )
      }
    }

}
