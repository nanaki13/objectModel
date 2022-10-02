package bon.jo.request

import org.scalajs.dom.XMLHttpRequest
import scala.concurrent.Future
import scala.concurrent.Promise
import scalajs.js
import HttpRequest.*
import scala.util.Success
import scala.util.Failure
import scala.util.Try
package simple:
  extension (m : Method)
    def apply(url : String,body : Option[String],headers : Map[String,String]): Future[Response] = 
      request(m,url,body,headers)
package predef:
  extension (m : Method)(using GlobalParam)
    def send(url : String,body : Option[String],headers : Map[String,String]): Future[Response] = 
      request(m,baseUrl+url,body,headers ++globalHeaders)
  extension (m : Get)(using GlobalParam)
    def apply(url : String,headers : Map[String,String]): Future[Response] = 
      m.send(url,None,headers ++ summon.headers)
object HttpRequest :
  given [T](using cv : Conversion[js.Any,T]) : Conversion[js.Any,Seq[T]] = e =>  conv(cv)(e.asInstanceOf[js.Array[js.Any]])
  def conv[T]( cv : Conversion[js.Any,T]) : Conversion[js.Array[js.Any],Seq[T]] = 
    e => e.map{
      f => cv(f)
    }.toSeq
  enum Method:
    case GET,POST,PUT
  type Get = Method.GET.type
  type Post = Method.POST.type
  type Put = Method.PUT.type


  extension (jsValue : js.Any)
    def toEntity[T](using  Conversion[js.Any,T]):T = summon(jsValue)
  extension [T](t : T)
    inline def toStringBody(using  Conversion[T,String]):String = summon(t)
    inline def toJs(using  Conversion[T,js.Any]):js.Any = summon(t)
  extension (t : String)
    inline def toEntity[T](using  Conversion[String,T]):T = summon(t)
  extension (response : Response)
    def mapStatus[T](f : PartialFunction[Int,T]):T=
      f.applyOrElse(response.status, s => throw BadStatusException[String](s"not valid : ${s}"))
      
    def okWith[OK,KO](status : Int)(using  Conversion[String,OK],Conversion[String,KO]):OK = 

      if response.status == status then
        response.value.toString().toEntity[OK]
      else 
        throw BadStatusException(response.value.toString().toEntity[KO])
      
    def okWithJs[OK,KO](status : Int)(using  Conversion[js.Any,OK],Conversion[String,KO]):OK = 
      given Conversion[String,js.Any] = js.JSON.parse(_)
      okWith(status).toEntity
  trait Service(using GlobalParam):
    val basePath : String
    import predef.*
    extension (m : Method)
      def sendOn(url : String,body : Option[String] = None,headers : Map[String,String] = Map.empty): Future[Response] = 
        m.send(basePath+url,body,headers ++ globalHeaders)
      def sendEntity[T](url : String,body : T = None,headers : Map[String,String] = Map.empty)(using Conversion[T,String]): Future[Response] = 
        m.send(basePath+url,Some(body.toStringBody),headers ++ globalHeaders)
      def sendJsEntity[T <: js.Any](url : String,body : T ,headers : Map[String,String] = Map.empty): Future[Response] = 
        m.send(basePath+url,Some(js.JSON.stringify(body)),headers ++ globalHeaders)
      def sendToJsEntity[T](url : String,body : T = None,headers : Map[String,String] = Map.empty)(using Conversion[T,js.Any]): Future[Response] = 
        m.sendJsEntity(url,body.toJs,headers ++ globalHeaders)


  case class Response(value : js.Any, status : Int, statusText : String,responseType : String)

  case class GlobalParam(baseUrl : String,headers : Map[String,String])

  def baseUrl(using GlobalParam): String = summon.baseUrl
  def globalHeaders(using GlobalParam): Map[String,String] = summon.headers
  def request(method : Method,url : String,body : Option[String],headers : Map[String,String]) : Future[Response] = 
    val oReq = new XMLHttpRequest()
    val ret = Promise[Response]()
    println((method : Method,url : String,body : Option[String],headers : Map[String,String]) )
    val sendOp =  Try{
      oReq.open(method.toString(), url, true);
      headers.foreach(oReq.setRequestHeader)
      oReq.onload = e => {

        ret.success( Response(oReq.response,oReq.status,oReq.statusText,oReq.responseType))
      }
      oReq.onerror= e => {

        ret.success( Response(oReq.response,oReq.status,oReq.statusText,oReq.responseType))
      }
      oReq.send(body.getOrElse(null));
    
    }
    sendOp match
      case Success(v) => ret.future
      case Failure(v) => Future.failed(v)
    
    
