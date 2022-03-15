package bon.jo
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.Element
import scala.language.dynamics
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.Event
import org.scalajs.dom.KeyboardEvent
import bon.jo.words.MathExp
import bon.jo.words.Phrase.ParsingException
import bon.jo.words.PhraseElement
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.window

import org.scalajs.dom.Text
import org.scalajs.dom.HTMLSpanElement
import org.scalajs.dom.Node
import org.scalajs.dom.console.log
import scala.collection.mutable
import MiniDsl as !
import !.*
 
object WordMain :

  
 

  @main
  def test() : Unit = 
    document.body.appendChild(!.div[HTMLDivElement]{childs(main,resMain,err)})

  val formuleEval: HTMLDivElement  = !.div[HTMLDivElement]
  val res : HTMLDivElement = !.div[HTMLDivElement]{_text("--");!._class("debug")}
  val value : HTMLDivElement = !.div[HTMLDivElement]{_text("--");!._class("formule")}
  val error : HTMLDivElement = !.div[HTMLDivElement](_text("Tapper une fonction ex: cos(x)"))
  val input : LiveText = LiveText("span","formule")(change)
  val main: HTMLDivElement  = !.div[HTMLDivElement]{
  !._class("container top")
  childs(
    input.out,!.span[HTMLSpanElement](_text("=")),value,formuleEval)
  }
  val err: HTMLDivElement  = !.div[HTMLDivElement]{ !._class("container")
  childs(
    error)}
    val resMain: HTMLDivElement  = !.div[HTMLDivElement]{ !._class("container")
  childs(
    res)}
  


  def change(lt : LiveText,out : HTMLElement):Unit=
    val current = out.innerText
    try 
      val me = MathExp(current)    
      val compiled = me.compile()
      compiled.single match
        case Some(v) => 
          v match 
            case MathExp.NumberValue(n) => value.textContent =n.toString
            case f : MathExp.FunctionMathExp => 
              if f.symbols.isEmpty then 
                value.textContent =f.constantValue.value.toString
              else
                value.textContent = s"f(${f.symbols.map(_.value).mkString(",")})"
                val v = View(f)
                formuleEval.textContent = ""
                formuleEval.append(v.root)
            case o  =>  value.textContent ="cant display"
        case _ =>  value.textContent ="cant display" 
      res.textContent =compiled.toString() 
      
      
      if(error.textContent != "") then
        error.textContent = ""
        lt.resetFormat()

    catch
      case ParsingException(message,char) => 
        val carPos = lt.sizeToSel
        lt.inserted = false
        out.textContent = ""
        val s = char.map(v =>v.pos -> v).toMap
        val (buffer,_) = current.zipWithIndex.map{
          (e,i)=>
            s.get(i).map(v => (e ,i, Some(v))).getOrElse((e ,i, None))

        }.foldLeft((StringBuilder(),out)){
          case ((buffer,inbur),(char,index,erreur)) => 
            erreur match
              case Some(v) => 
                if buffer.length != 0
                then 
                  lt.add(!.span[HTMLSpanElement](_text(buffer.toString)),carPos)
                  buffer.length = 0

                lt.add(!.span[HTMLSpanElement]{
                  _text(char.toString)
                  _class("erreur")
                },carPos)
              
              case None => buffer.append(char)   
          

              
            (buffer,inbur)              
        }
        if buffer.length != 0
                then 
                  lt.add(!.span[HTMLSpanElement](_text(buffer.toString)),carPos)
        error.textContent =  "ParsingException  : "+ message + char.map(v=> s" at token : '${v.value.value}' position : ${v.pos}").mkString
        
      case (e : Exception) => 
        error.textContent = "Exception : "+e.getMessage+" "+e.getClass

    


