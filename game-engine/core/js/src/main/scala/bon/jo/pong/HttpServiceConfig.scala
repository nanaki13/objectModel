package bon.jo.pong

import bon.jo.request.HttpRequest.GlobalParam
import Login.UserContext.*

object HttpServiceConfig:
  given GlobalParam = GlobalParam("http://localhost:8080", Map.empty)

  object AuthParam:
    given (using Login.UserContext) :  GlobalParam = GlobalParam("http://localhost:8080", Map("Authorization" -> s"Bearer $token"))   
