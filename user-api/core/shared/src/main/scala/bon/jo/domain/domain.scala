package bon.jo

package  domain:
  case class User(id : Long,name : String,pwd : String)
  case class UserLogin(name : String,pwd : String)
  case class UserInfo(id : Long,name : String)
