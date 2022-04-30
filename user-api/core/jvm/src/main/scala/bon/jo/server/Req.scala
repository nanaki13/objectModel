package bon.jo.server

object Req:
  case class UserLogin(name : String,pwd : String)
