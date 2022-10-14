package bon.jo

package  domain:
  case class User(id : Long,name : String,pwd : String)
  case class UserLogin(name : String,pwd : String)
  case class UserInfo(id : Long,name : String)
  case class Image(id : Long, name : String, data : Array[Byte])
  case class ImageInfo(id : Long, name : String)
  case class ImageSend(name : String, data : Array[Byte])
