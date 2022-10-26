package bon.jo.html
import org.scalajs.dom.window
object TimeEffect {
  
}
trait TimeEffect:
  val interval : Int
  var int = 0
  def process():Unit
  def haveToStop():Boolean
  def onStop() : Option[TimeEffect] = None

  def start():Unit =  
    int = window.setInterval(() => processIfNotStop(),interval)

  def processIfNotStop():Unit = 
    if haveToStop() then
      window.clearInterval(int)
      onStop().foreach(_.start())
    else process()
