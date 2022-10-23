package bon.jo.pong

import bon.jo.request.HttpRequest.GlobalParam
import Login.UserContext.*

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
    given (using Login.UserContext) :  GlobalParam = GlobalParam(Globals.host, Map("Authorization" -> s"Bearer $token"))   
