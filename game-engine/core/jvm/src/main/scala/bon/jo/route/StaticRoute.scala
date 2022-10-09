package bon.jo.route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
object StaticRoute:
    def apply(): Route = pathPrefix("break-broke") {
    getFromResourceDirectory("break-broke")
  }
