package bon.jo

package  domain:
  case class Id(id : Long)
  object Id:
    given Conversion[Long,Id] = Id(_)
    given Conversion[Option[Long],Option[Id]] = _.map(e => e.convert)
    given Conversion[Id,Long] = _.id
  case class User(id : Long,name : String,pwd : String,avatarKey : Option[Long])
  case class UserAvatar(user : User,avatar : Option[ImageInfo])
  case class UserLogin(name : String,pwd : String)
  case class UserInfo(id : Long,name : String,avatar : Option[ImageInfo])
  case class Image(id : Long, name : String, data : Array[Byte])
  case class ImageInfo(id : Long, name : String)
  case class ImageSend(name : String, data : Array[Byte])
  case class UserContext(user: UserInfo, token: String):
    def this(token: String,tokenExtract : String => UserInfo) =
      this(tokenExtract(token), token)
  object UserContext:
    type Ctx[A] = UserContext ?=> A
    inline def apply(): Ctx[UserContext] = summon
    inline def user: Ctx[UserInfo] = UserContext().user
    inline def token: Ctx[String] = UserContext().token

  enum Response:
    case OK
    case KO(reason: String)
