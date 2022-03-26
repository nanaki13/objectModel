package bon.jo.home

import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLElement
import scala.concurrent.Future
import scala.concurrent.Promise
import org.scalajs.dom.HTMLInputElement
import concurrent.ExecutionContext.Implicits.global

trait StepAsk[T,F <: HTMLElement]:
  val v : HTMLButtonElement
  val toAsk : F
  val validToAsk : HTMLButtonElement
  def readAsk(e : F) : T
  private val parent = v.parentElement
  v.onclick= _ =>
    println("CLICK")
    parent.removeChild(v)
    parent.append(toAsk,validToAsk)
    validToAsk.onclick = _ => 
      value(readAsk(toAsk))
      parent.removeChild(toAsk)
      parent.removeChild(validToAsk)
      parent.append(v)
  
  def value(t : T ) : Unit
   

object StepAsk :
  class SimpleStepAsk(val v : HTMLButtonElement,val toAsk : HTMLInputElement,val validToAsk : HTMLButtonElement)(valueFunc : (value : String) => Unit) extends  StepAsk[String,HTMLInputElement]:
    def  readAsk(e : HTMLInputElement) :String = e.value

    def value(t : String) = valueFunc(t)
