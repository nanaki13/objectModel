package bon.jo.pong

import bon.jo.request.HttpRequest.GlobalParam
import Login.UserContext.*

object HttpServiceConfig:
  //val host = "https://nanaki.hd.free.fr"
  val host = "https://localhost:8080"
  given GlobalParam = GlobalParam(host, Map.empty)

  object AuthParam:
    given (using Login.UserContext) :  GlobalParam = GlobalParam(host, Map("Authorization" -> s"Bearer $token"))   
