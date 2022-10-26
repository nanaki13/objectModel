package bon.jo.pong
import Store.*
import bon.jo.html.Html.*
import bon.jo.common.typeutils.~
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document.body
import org.scalajs.dom.window
import scala.concurrent.Future
import scala.concurrent.Promise
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLSpanElement
import bon.jo.html.request.HttpRequest
import bon.jo.html.request.HttpRequest.Method.{POST, GET}
import scalajs.js
import scalajs.js.UndefOrOps
import concurrent.ExecutionContext.Implicits.global
import scala.util.Success.apply
import scala.util.Success
import scala.util.Failure
import bon.jo.html.request.BadStatusException
import bon.jo.html.request.BadStatusExceptionValue
import org.scalajs.dom.HTMLAnchorElement
import bon.jo.Validator.*
import bon.jo.Validator
import org.scalajs.dom.HTMLLabelElement
import org.scalajs.dom.HTMLFormElement
import bon.jo.domain.UserLogin
import bon.jo.domain.UserInfo
import java.util.Base64
import org.scalajs.dom.FormData
import bon.jo.html.request.HttpRequest.GlobalParam
import bon.jo.html.HttpServiceConfig
import bon.jo.html.HttpServiceConfig.AuthParam.given
import bon.jo.domain.Id
import bon.jo.domain.ImageInfo
import bon.jo.domain.UserContext


object Login:

  given Validator[UserLogin] = Validator { u =>
    (if u.name.isBlank() then ValidationResult.Failure(u, "give a name")
     else ValidationResult.Success(u))
      .and(
        if u.pwd.isBlank() then ValidationResult.Failure(u, "\ngive a password")
        else ValidationResult.Success(u)
      )
  }
  given Conversion[js.Any,UserInfo] = 
      e => userInfo(e.asInstanceOf[UserInfoJs])
  trait ImageInfoJs extends js.Object:
    val id : Double
    val name: String
  object ImageInfoJs:
    def unapply(e : ImageInfoJs):ImageInfo = ImageInfo(e.id.toLong,e.name)
   
  trait UserLoginJs extends js.Object:
    val name: String
    val pwd: String
  trait UserInfoJs extends js.Object:
    val id: Double
    val name: String
    val avatar: js.UndefOr[ImageInfoJs]
  object UserLoginJs:
    def apply(userLogin: UserLogin): UserLoginJs =
      js.Dynamic
        .literal(name = userLogin.name, pwd = userLogin.pwd)
        .asInstanceOf

  given Conversion[String, String] = e => e
  val gb: GlobalParam = {
    import HttpServiceConfig.given
    summon
  }
  object baseTokenService extends HttpRequest.Service(using gb):
    val basePath: String = "/token"
    def getToken(user: UserLogin): Future[String] =
      POST
        .sendJsEntity("", UserLoginJs(user))
        .map(_.okWith[String, String](200))
  class TokenServicePrivate()(using UserContext) extends HttpRequest.Service:
    val basePath: String = "/token"
    def refresh(): Future[String] =
      GET.sendOn("/refresh").map(_.okWith[String, String](200))
  object userServicePublique extends HttpRequest.Service(using gb):
    val basePath: String = "/users"
    def createAccount(user: UserLogin): Future[String] =
      POST
        .sendJsEntity("", UserLoginJs(user))
        .map(_.okWith[String, String](200))

  class UserServicePrivate()(using UserContext) extends HttpRequest.Service:
    val basePath: String = "/users"
    def sendAvatar(data: FormData) =
      POST.sendOn("/avatar", Some(data))
    def me():Future[UserInfo] = 
      GET.sendOn(s"/${UserContext.user.id}").map(_.okWithJs[UserInfo,String](200)) 
  given (using UserContext): UserServicePrivate = UserServicePrivate()
  given (using UserContext): TokenServicePrivate = TokenServicePrivate()
  def myAvatarUrl(using c: UserContext, g: GlobalParam): String =
    avatartUrl(c.user)
  def avatartUrl(user: UserInfo)(using g: GlobalParam) =
    s"${user.avatar.map(name => s"${g.baseUrl}/images/${name.name}").getOrElse("./assets/img/no-avatar.webp")}"
  inline def userServicePrivate: ~[UserServicePrivate] = summon
  inline def tokenServicePrivate: ~[TokenServicePrivate] = summon
  def userInfo(jsObj: UserInfoJs): UserInfo =
    UserInfo(
      id = jsObj.id.toLong,
      name = jsObj.name,
      avatar = jsObj.avatar.toOption.map(ImageInfoJs.unapply)
    )
  def userInfoFromString(token: String): UserInfo = userInfo(
    js.JSON
      .parse(new String(Base64.getUrlDecoder().decode(token.split("\\.")(1))))
      .asInstanceOf[UserInfoJs]
  )

  val tokenKey = "token"
  def logOut(): Unit =
    tokenKey.storageRemove
    window.location.reload(true)
  def logoutButton(): HTMLButtonElement =
    <.button[HTMLButtonElement](text("Exit"))
      .>(_.onclick = e => {

        logOut()
      })

  def refreshToken(): UserContext ?=> Future[UserContext] =
    tokenServicePrivate.refresh().map { token =>
      token.storageWrite(tokenKey)
      new UserContext(token, userInfoFromString)
    }

  def log(): Future[UserContext] =
    val pro: Promise[UserContext] = Promise()
    val tokenOption = tokenKey.storageRead
    val loginText = "Login"
    val createText = "Create"
    tokenOption match
      case Some(token) =>
        given UserContext = new UserContext(token, userInfoFromString)
        try
          refreshToken().map { noken =>
            pro.success(noken)
          } .onComplete{
            
              case Failure(exception) => 
                exception.printStackTrace()
                logOut()
              case Success(value) =>
            
          }
        catch
          case e =>
            e.printStackTrace()
            logOut()

      case None =>
        val inp = <.input[HTMLInputElement].>(_.id = "name", _.name = "name")
        val inpwd = <.input[HTMLInputElement].>(
          _.`type` = "password",
          _.id = "pwd",
          _.name = "pwd"
        )
        val title = <.div[HTMLElement](text("Login"))

        val ok: HTMLButtonElement =
          <.button[HTMLButtonElement](text(loginText)).>(_.`type` = "submit")
        val createAccount: HTMLElement = <.div[HTMLElement](
          childs(<.a[HTMLAnchorElement](text("Create Account")).>(_.href = "")),
          _class("create-account")
        )
      //  val a: HTMLLabelElement = null

        val bottom =
          <.div[HTMLElement](childs(ok, createAccount), _class("p-relative"))
        val diagPs =
          <.div[HTMLElement](
            childs(
              <.div[HTMLElement](childs(title)),
              <.div[HTMLElement](
                childs(
                  <.label[HTMLLabelElement](text("Pseudo"))
                    .>(_.htmlFor = "name"),
                  inp
                )
              ),
              <.div[HTMLElement](
                childs(
                  <.label[HTMLLabelElement](text("Password"))
                    .>(_.htmlFor = "pwd"),
                  inpwd
                )
              ),
              bottom
            ),
            _class("dialog")
          )
        def removeWarn() = bottom
          .getElementsByClassName("login-warn")
          .foreach(bottom.removeChild)
        val form = <.form[HTMLFormElement](childs(diagPs))
          .>(_.name = "form", _.name = "form")
        val diagc = <.div[HTMLElement](childs(form), _class("flex-center"))
        val diag = <.div[HTMLElement](childs(diagc), _class("splash"))

        def onClickForCreateAccount(): Unit = {
          removeWarn()
          try
            userServicePublique
              .createAccount(UserLogin(inp.value, inpwd.value).validate())
              .onComplete {
                case Success(result) =>
                  title.textContent = "User created"
                  title.classList.add("ok")
                  title :+ <.div[HTMLElement](
                    text(s"You can login (click $loginText)")
                  )
                  ok.textContent = loginText
                  ok.onclick = e =>
                    e.preventDefault()

                    onClickForToken()
                case Failure(BadStatusException(value)) =>
                  bottom :+ <.span[HTMLElement](
                    text(value.toString()),
                    _class("login-warn ko")
                  )
                case o =>
                  removeWarn()
              }
          catch
            case ValidatorException(v, message) =>
              val childsL =
                message.split("\n").map(e => <.div[HTMLElement](text(e)))
              bottom :+ <.div[HTMLElement](
                childs(childsL*),
                _class("login-warn ko")
              )
        }
        def onClickForToken(): Unit = {
          removeWarn()
          try
            baseTokenService
              .getToken(UserLogin(inp.value, inpwd.value).validate())
              .onComplete {
                case Success(token) =>
                  body.removeChild(diag)
                  token.storageWrite(tokenKey)

                  pro.success(new UserContext(token, userInfoFromString))
                case Failure(BadStatusExceptionValue[String](value)) =>
                  val txt =
                    if value.isEmpty then "Something wrong inside..." else value
                  bottom :+ <.span[HTMLElement](
                    text(txt),
                    _class("login-warn ko")
                  )
                case Failure(e) =>
                  bottom :+ <.span[HTMLElement](
                    text(e.toString()),
                    _class("login-warn ko")
                  )
              }
          catch
            case ValidatorException(v, message) =>
              val childsL =
                message.split("\n").map(e => <.div[HTMLElement](text(e)))
              bottom :+ <.div[HTMLElement](
                childs(childsL*),
                _class("login-warn ko")
              )
            case e =>
              bottom :+ <.span[HTMLElement](
                text(e.toString()),
                _class("login-warn ko")
              )
        }

        createAccount.onclick = e => {
          e.preventDefault()
          removeWarn()
          title.textContent = "Create Account"
          ok.textContent = createText
          ok.onclick = e =>
            e.preventDefault()
            onClickForCreateAccount()
          createAccount.parentNode.removeChild(createAccount)
        }

        ok.onclick = e =>
          e.preventDefault()
          onClickForToken()
        body.append(diag)
    pro.future
