package bon.jo.pong

import bon.jo.request.HttpRequest.GlobalParam
import Login.UserContext.*

object HttpServiceConfig:
  val host = "http://82.64.146.186"
  given GlobalParam = GlobalParam(host, Map.empty)

  object AuthParam:
    given (using Login.UserContext) :  GlobalParam = GlobalParam(host, Map("Authorization" -> s"Bearer $token"))   
