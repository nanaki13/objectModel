package bon.jo.html

import bon.jo.html.request.HttpRequest.GlobalParam
import bon.jo.domain.UserContext.*
import bon.jo.domain.UserContext
import scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
object HttpServiceConfig:
  @js.native
  @JSGlobalScope
  object Globals extends js.Object {
    val host: String = js.native
  }

  given GlobalParam = GlobalParam(Globals.host, Map.empty)

  object AuthParam:
    given (using UserContext) :  GlobalParam = GlobalParam(Globals.host, Map("Authorization" -> s"Bearer $token"))   
