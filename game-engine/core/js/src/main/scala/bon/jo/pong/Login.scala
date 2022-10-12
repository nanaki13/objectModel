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
import bon.jo.request.BadStatusExceptionValue
import javax.swing.text.html.HTML
import org.scalajs.dom.HTMLAnchorElement
import bon.jo.Validator.*
import bon.jo.Validator
import org.scalajs.dom.HTMLLabelElement
import org.scalajs.dom.HTMLFormElement
import bon.jo.domain.UserLogin
import bon.jo.domain.UserInfo
import java.util.Base64
object Login:

  import HttpServiceConfig.given

  given Validator[UserLogin]  = Validator{
    u => 
      (if u.name.isBlank() then ValidationResult.Failure(u,"give a name") else ValidationResult.Success(u)) 
      .and(if u.pwd.isBlank() then ValidationResult.Failure(u,"\ngive a password") else ValidationResult.Success(u))
  }
  trait UserLoginJs extends js.Object:
    val name: String
    val pwd: String
  trait UserInfoJs extends js.Object:
    val id: Double
    val name: String
  object UserLoginJs:
    def apply(userLogin : UserLogin): UserLoginJs =
      js.Dynamic.literal(name = userLogin.name, pwd = userLogin.pwd).asInstanceOf
 
  given Conversion[String, String] = e => e
  object baseTokenService extends HttpRequest.Service:
    val basePath: String = "/token"
    def getToken(user: UserLogin): Future[String] =
      POST.sendJsEntity("", UserLoginJs(user)).map(_.okWith[String, String](200))

  object userService extends HttpRequest.Service:
    val basePath: String = "/users"
    def createAccount(user: UserLogin): Future[String] =
      POST.sendJsEntity("", UserLoginJs(user)).map(_.okWith[String, String](200))
  def userInfo(jsObj : UserInfoJs) : UserInfo = 
    UserInfo(id = jsObj.id.toLong, name = jsObj.name)  
  def userInfo(token : String) : UserInfo =  userInfo(js.JSON.parse(new String( Base64.getUrlDecoder().decode(token.split("\\.")(1)))).asInstanceOf[UserInfoJs])
  

  val tokenKey = "token"
  def logoutButton(): HTMLButtonElement =
    <.button[HTMLButtonElement](text("Exit"), _class("top-right"))
      .>(_.onclick = e => {
       
        tokenKey.storageRemove
        window.location.reload(true)
      })
  case class UserContext(user : UserInfo,token : String):
    def this(token : String) =
      this(userInfo(token),token)
  object UserContext:
    type ~[A] = UserContext ?=> A
    inline def apply() :  ~[UserContext] = summon
    inline def user : ~[UserInfo] = UserContext().user
    inline def token : ~[String] = UserContext().token
  def log(): Future[UserContext] =
    val pro: Promise[UserContext] = Promise()
    val tokenOption = tokenKey.storageRead
    val loginText = "Login"
    val createText = "Create"
    tokenOption match
      case Some(token) => pro.success(new UserContext(token))
      case None =>
        val inp = <.input[HTMLInputElement].>(_.id = "name",_.name = "name")
        val inpwd = <.input[HTMLInputElement].>(_.`type` = "password",_.id = "pwd",_.name = "pwd")
        val title = <.div[HTMLElement](text("Login"))
        
        val ok: HTMLButtonElement = <.button[HTMLButtonElement](text(loginText)).>(_.`type`="submit")
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
                  title.textContent = "User created"
                  title.classList.add("ok")
                  title :+ <.div[HTMLElement](text(s"You can login (click $loginText)"))
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
              case ValidatorException(v ,message) => 
                val childsL = message.split("\n").map(e =>  <.div[HTMLElement](
                text(e)))
                bottom :+ <.div[HTMLElement](
                  childs(childsL *),
                  _class("login-warn ko"))
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
                  
                  pro.success(new UserContext(token))
                case Failure(BadStatusExceptionValue[String](value)) =>
               
           
                  
                  
                  val txt = if value.isEmpty then "Something wrong inside..." else value
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
            case ValidatorException(v ,message) => 
              val childsL = message.split("\n").map(e =>  <.div[HTMLElement](
                text(e)))
              bottom :+ <.div[HTMLElement](
                childs(childsL *),
                _class("login-warn ko"))
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
