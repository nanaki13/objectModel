package bon.jo.html
import scala.language.dynamics

import org.scalajs.dom.Element
import org.scalajs.dom.document
  
object Create extends scala.Dynamic:

  def applyDynamic[T <: Element](tag : String)(f : T ?=> Unit *) : T = 
    given t : T = document.createElement(tag).asInstanceOf
    f.foreach(ff => ff)
    t
  def selectDynamic[T <: Element](tag : String) : T = 
    document.createElement(tag).asInstanceOf[T]
