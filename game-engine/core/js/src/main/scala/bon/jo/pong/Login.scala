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
object Login : 
  val pseudoKey = "pseudo"
  def logoutButton(onlogOut : => Unit): HTMLButtonElement = 
    <.button[HTMLButtonElement](text("Exit"),_class("top-right")).>(_.onclick = e => {
      pseudoKey.storageRemove
      onlogOut
      window.location.reload(true)
    })
  
  def log():Future[String] = 
    val pro : Promise[String] = Promise()
    val pseudo = pseudoKey.storageRead
    pseudo match
      case Some(p) => pro.success(p)
      case None => 
        val inp = <.input[HTMLInputElement]
        val ok : HTMLButtonElement = <.button[HTMLButtonElement](text("OK"))
        val diagS =   <.div[HTMLElement](childs(<.span[HTMLSpanElement](text("Pseudo")),inp,ok),_class("dialog"))
        val diagc =   <.div[HTMLElement](childs(diagS),_class("flex-center"))
        val diag = <.div[HTMLElement](childs(diagc),_class("splash"))
        ok.onclick = e => {
          body.removeChild(diag) 
          inp.value.storageWrite(pseudoKey)
          pro.success(inp.value)      
        }
        body.append(diag)
    pro.future
    
 
