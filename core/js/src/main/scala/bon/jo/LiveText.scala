package bon.jo

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.Node
import org.scalajs.dom.document
import org.scalajs.dom.console.log
import org.scalajs.dom.Text
import org.scalajs.dom.KeyboardEvent

case class LiveText(tag : String,class_ : String)(f : (me : LiveText,outp : HTMLElement)=>Unit):
  extension (n : Node)
    def allChild():scala.collection.Seq[Node] = 
      n.childNodes.flatMap{ e =>
        e +: e.allChild()
      }
      
  val ! = MiniDsl
  import !.*
  var inserted = false
  def add(s : Node,carPos:Int):Unit  = 
    out.append(s)
    check(s,carPos)
  def check(s : Node,carPos:Int) :Unit=   
    if !inserted && carPos <= out.textContent.size then
      out.focus() 
      sel.collapse(s.childNodes.head,carPos - (out.textContent.size - s.textContent.size))
      inserted = true
      
  
  val out : HTMLElement  = !.applyDynamic[HTMLElement](tag)(_text("0"))
  out.contentEditable = "true"
  out.className = class_
  var prev = false
  inline def sel = document.getSelection()
  def resetFormat() = 
    val carPos = sizeToSel
    out.textContent = out.textContent
    sel.collapse(out.childNodes.head,carPos)
  def sizeToSel : Int = 
    val (focusNode,focusOffset) =( sel.focusNode, sel.focusOffset ) ;
    out.allChild().takeWhile(_ != focusNode).map{
      case t : Text => t.textContent.size
      case _ => 0
    }.sum+sel.focusOffset

 
  out.onkeyup = (e : KeyboardEvent) =>
    log(e)
    if(e.key.length == 1 || e.keyCode == 8)
       f(this,out) 

    