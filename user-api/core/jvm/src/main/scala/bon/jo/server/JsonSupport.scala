package bon.jo.server

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, RequestEntity}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import org.json4s.Formats

import scala.concurrent.{ExecutionContext, Future}
import java.time.format.DateTimeFormatter
import org.json4s.CustomSerializer
import java.time.LocalDate
import java.time.LocalDateTime
import org.json4s.JString
import org.json4s.JNull

object JsonSupport:
  given  [A](using  Materializer,Manifest[A],Manifest[Seq[A]],Formats) : JsonSupport[A] = new JsonSupport[A]{}

  def apply[A]()(using  JsonSupport[A]):JsonSupport[A] = summon

  def format = org.json4s.DefaultFormats ++ CustomSerializers()
  object CustomSerializers {

    private val dateFormatter     = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

    def apply() = Seq(LocalDateSerializer,LocalDateTimeSerializer)
    case object LocalDateSerializer extends CustomSerializer[LocalDate](format => ( {
      case JString(date) => LocalDate.parse(date, dateFormatter)
      case JNull         => null
    }, {
      case date: LocalDate => JString(date.format(dateFormatter))
    }))

    case object LocalDateTimeSerializer extends CustomSerializer[LocalDateTime](format => ( {
      case JString(dt) => LocalDateTime.parse(dt, dateTimeFormatter)
      case JNull       => null
    }, {
      case dt: LocalDateTime => JString(dt.format(dateTimeFormatter))
    }))
}

trait JsonSupport[A](using Materializer,Manifest[A],Manifest[Seq[A]],Formats ) {

  def entityConv[B](a : B): HttpEntity.Strict = {
    HttpEntity(contentType = ContentTypes.`application/json`, string = org.json4s.native.Serialization.write(a))
  }
  def fromentity[B](entity : HttpEntity)(implicit  manifest: Manifest[B],executionContext: ExecutionContext):Future[B]= {
    entity.dataBytes.runFold("")((res, bs) => s"$res${bs.utf8String}").map(org.json4s.native.Serialization.read[B])
  }
  given ToEntityMarshaller[A] = Marshaller
    .opaque[A, RequestEntity](
      resp => {
        entityConv(resp)
      })

  given  ToEntityMarshaller[Iterable[A]] = Marshaller
    .opaque[Iterable[A], RequestEntity](
      resp => {
        entityConv(resp)
      })
  // marshalling would usually be derived automatically using libraries
  given FromEntityUnmarshaller[A] = {
    Unmarshaller[HttpEntity, A]( ec => memoRequest =>
      given  ExecutionContext = ec
      fromentity[A](memoRequest))
  }
  given  FromEntityUnmarshaller[Seq[A]] = {
    Unmarshaller[HttpEntity, Seq[A]]( e => memoRequest =>
      given  ExecutionContext = e
      fromentity[Seq[A]](memoRequest))
  }
}