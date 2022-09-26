package bon.jo.pong
import Store.*
import bon.jo.html.Html.*
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document.body
import org.scalajs.dom.window
import scala.concurrent.Future
import scala.concurrent.Promise
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLSpanElement
import bon.jo.request.HttpRequest
import bon.jo.request.HttpRequest.Method.POST
import scalajs.js
import concurrent.ExecutionContext.Implicits.global
import scala.util.Success.apply
import scala.util.Success
import scala.util.Failure
import bon.jo.request.BadStatusException
import javax.swing.text.html.HTML
import org.scalajs.dom.HTMLAnchorElement
import bon.jo.Validator.*
import bon.jo.Validator
import org.scalajs.dom.HTMLLabelElement
import org.scalajs.dom.HTMLFormElement
object Login:

  import HttpServiceConfig.given

  given Validator[UserLogin]  = Validator{
    u => 
      (if u.name.isBlank() then ValidationResult.Failure(u,"give a name") else ValidationResult.Success(u)) 
      .and(if u.pwd.isBlank() then ValidationResult.Failure(u,"\ngive a password") else ValidationResult.Success(u))
  }
  trait UserLogin extends js.Object:
    val name: String
    val pwd: String
  object UserLogin:
    def apply(name: String, pwd: String): UserLogin =
      js.Dynamic.literal(name = name, pwd = pwd).asInstanceOf
  class UserInfo(name: String) extends js.Object
  given Conversion[String, String] = e => e
  object baseTokenService extends HttpRequest.Service:
    val basePath: String = "/token"
    def getToken(user: UserLogin): Future[String] =
      POST.sendJsEntity("", user).map(_.okWith[String, String](200))

  object userService extends HttpRequest.Service:
    val basePath: String = "/users"
    def createAccount(user: UserLogin): Future[String] =
      POST.sendJsEntity("", user).map(_.okWith[String, String](200))

  val pseudoKey = "pseudo"
  val tokenKey = "token"
  def logoutButton(onlogOut: => Unit): HTMLButtonElement =
    <.button[HTMLButtonElement](text("Exit"), _class("top-right"))
      .>(_.onclick = e => {
        pseudoKey.storageRemove
        tokenKey.storageRemove
        onlogOut
        window.location.reload(true)
      })

  def log(): Future[String] =
    val pro: Promise[String] = Promise()
    val pseudo = pseudoKey.storageRead
    pseudo match
      case Some(p) => pro.success(p)
      case None =>
        val inp = <.input[HTMLInputElement].>(_.id = "name",_.name = "name")
        val inpwd = <.input[HTMLInputElement].>(_.`type` = "password",_.id = "pwd",_.name = "pwd")
        val title = <.span[HTMLSpanElement](text("Login"))
        val ok: HTMLButtonElement = <.button[HTMLButtonElement](text("OK")).>(_.`type`="submit")
        val createAccount: HTMLElement = <.div[HTMLElement](
          childs(<.a[HTMLAnchorElement](text("Create Account")).>(_.href = "")),
          _class("create-account")
        )
        val a : HTMLLabelElement = null
      
        val bottom =
          <.div[HTMLElement](childs(ok, createAccount), _class("p-relative"))
        val diagPs =
          <.div[HTMLElement](
            childs(
              <.div[HTMLElement](childs(title)),
              <.div[HTMLElement](
                childs(<.label[HTMLLabelElement](text("Pseudo")).>(_.htmlFor = "name"), inp)
              ),
              <.div[HTMLElement](
                childs(<.label[HTMLLabelElement](text("Password")).>(_.htmlFor = "pwd"), inpwd)
              ),
              bottom
            ),
            _class("dialog")
          )
        def removeWarn() = bottom
          .getElementsByClassName("login-warn")
          .foreach(bottom.removeChild)
        val form =  <.form[HTMLFormElement](childs(diagPs)).>(_.name="form",_.name = "form")
        val diagc = <.div[HTMLElement](childs(form), _class("flex-center"))
        val diag = <.div[HTMLElement](childs(diagc), _class("splash"))

        def onClickForCreateAccount(): Unit = {
          removeWarn()
          try
            userService
              .createAccount(UserLogin(inp.value, inpwd.value).validate())
              .onComplete {
                case Success(result) =>
                  title.textContent = "You can Login"
                  ok.onclick = e => onClickForToken()
                case Failure(BadStatusException(value)) =>
                  bottom :+ <.span[HTMLElement](
                    text(value.toString()),
                    _class("login-warn")
                  )
                case o =>
                  removeWarn()
              }
            catch
              case ValidatorException(v ,message) => 
                val childsL = message.split("\n").map(e =>  <.div[HTMLElement](
                text(e)))
                bottom :+ <.div[HTMLElement](
                  childs(childsL *),
                  _class("login-warn"))
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
                  inp.value.storageWrite(pseudoKey)
                  pro.success(inp.value)
                case Failure(BadStatusException(value)) =>
                  bottom :+ <.span[HTMLElement](
                    text(value.toString()),
                    _class("login-warn")
                  )
                case o =>
                  removeWarn()
              }
          catch
            case ValidatorException(v ,message) => 
              val childsL = message.split("\n").map(e =>  <.div[HTMLElement](
                text(e)))
              bottom :+ <.div[HTMLElement](
                childs(childsL *),
                _class("login-warn"))
        }

        createAccount.onclick = e => {
          e.preventDefault()
          removeWarn()
          title.textContent = "Create Account"
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