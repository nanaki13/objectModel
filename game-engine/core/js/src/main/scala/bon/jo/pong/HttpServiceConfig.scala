package bon.jo.pong

import bon.jo.request.HttpRequest.GlobalParam

object HttpServiceConfig:
  given GlobalParam = GlobalParam("http://localhost:8080", Map.empty)
