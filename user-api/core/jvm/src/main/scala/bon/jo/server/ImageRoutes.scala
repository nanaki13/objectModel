package bon.jo.server

import akka.actor.typed.ActorSystem
import akka.util.Timeout

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.Directive
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
import bon.jo.domain.Response
import bon.jo.domain.Response.*
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

  def extractData(
      fieldName: String
  ): (Materializer, ExecutionContext) ?=> Directive[(String, Array[Byte])] =
    entity(as[Multipart.FormData]).flatMap { formData =>

      // collect all parts of the multipart as it arrives into a map
      val allPartsF: Future[(String, Array[Byte])] = formData.parts
        .mapAsync[Array[Byte] | String | None.type](1) {

          case b: BodyPart if b.name == fieldName =>
            b.entity.dataBytes
              .runFold(ArrayBuffer[Byte]())((buff, data) => buff.addAll(data))
              .map(_.toArray)

          case b: BodyPart if b.name == "ext" =>
            b.entity.toStrict(2.seconds).map { s =>
              s.data.utf8String
            }

          case b: BodyPart =>
            b.entity.discardBytes()
            Future.successful(None)

        }
        .runFold("" -> Array[Byte]())((map, tuple) =>
          tuple match
            case s: String      => map.copy(_1 = s)
            case s: Array[Byte] => map.copy(_2 = s)
            case None           => map
        )

      onSuccess(allPartsF)

    }
class ImageRoutes(buildImageRepository: ActorRef[ImageRepo.Command])(using
    ActorRef[TokenRepo.Command],
    ActorSystem[_],
    Formats,ExecutionContext,Timeout
) extends JsonSupport[Image]
    with CORSHandler
    with TokenRouteGuard {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable


  lazy val theImageRoutes: Route =
    corsHandler {
      pathPrefix("images") {
        concat(
          (get & path(Segment)) { name =>
            rejectEmptyResponse {
              val maybeImage: Future[Option[Image]] =
                buildImageRepository.ask(Command.FindImages(name, _))

              val httpEntity = for {
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
