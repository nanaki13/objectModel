package bon.jo.user.login
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import bon.jo.HtmlEvent.events
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.document

import scalajs.js.Dynamic.literal
import scalajs.js.Dynamic
import scalajs.js.JSON
object UserLogin {
  @main
  def main():Unit = ()
  val okButton = button(_text("Login"))
  val loginInput = input(me(_.placeholder = "login"))
  val passwordInput = input(_text("name"),me(_.`type`="password"),me(_.placeholder = "password"))
  okButton.events.click{
    cl => 
      var oReq = new XMLHttpRequest();

      oReq.open("POST", "http://localhost:8080/auth", true);
      oReq.onload = e => {
        var arraybuffer = oReq.response; // n'est pas responseText
        /* ... */
      }
      oReq.send(JSON.stringify( literal(name = loginInput.value,pwd = passwordInput.value )));
  }
  document.body.append{
    div{
      childs(
      div(childs(loginInput)),
      div(childs(passwordInput)),
      div(childs(okButton))
      )
      
    }
  }

}
