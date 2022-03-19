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
import HtmlPredef.*
object WordMain :

  case class MyMenuItem(text : String, view : () => HTMLElement) extends Menu.MenuItem
 
  def graphView() = div{childs(main,resMain,err)}
  @main
  def test() : Unit = 
    val menuOut = div(_class("menu-out"))
    

    val graphMenu : MyMenuItem = new MyMenuItem("Fonction graph",graphView)
    val home : MyMenuItem = new MyMenuItem("Home",() => 
      div(_class("welcome container"),childs(
        !.h1[HTMLElement](_class(""),_text("Site perso avec des réalisations en Scala")),
        div(_class(""),childs(t("Mon github : "),a(me(_.href="https://github.com/nanaki13"),_text("https://github.com/nanaki13"))))
      ))
      
      
      
      )
    val menu  = new Menu(List(home,graphMenu),_.view(),menuOut) 
     
    
    val root = div(_class("root"),childs(menu.root,menuOut))
    
    
    document.body.appendChild(root)

  val formuleEval: HTMLDivElement  = div
  val res : HTMLDivElement = div(_text("--"),_class("debug"))
  val value : HTMLDivElement = div(_text("--"),_class("formule"))
  val error : HTMLDivElement = div(_text("Tapper une fonction ex: cos(x)"))
  val input : LiveText = LiveText("span","formule")(change)
  val main: HTMLDivElement  = div{
  _class("container top")
  childs(
    input.out,span(_text("=")),value,formuleEval)
  }
  val err: HTMLDivElement  = div{ _class("container")
  childs(
    error)}
    val resMain: HTMLDivElement  = div{ _class("container")
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
            case a@MathExp.Symbol(s,_) =>
              value.textContent = s"f($s)"
              val v = View(a.asFunction())
              formuleEval.textContent = ""
              formuleEval.append(v.root)  
            case o  =>  
              value.textContent ="cant display"
              println(o)
        case _ =>  
          value.textContent ="cant display" 
          println("compiled : "+compiled)
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
                  lt.add(span(_text(buffer.toString)),carPos)
                  buffer.length = 0

                lt.add(span{
                  _text(char.toString)
                  _class("erreur")
                },carPos)
              
              case None => buffer.append(char)   
          

              
            (buffer,inbur)              
        }
        if buffer.length != 0
                then 
                  lt.add(span(_text(buffer.toString)),carPos)
        error.textContent =  "ParsingException  : "+ message + char.map(v=> s" at token : '${v.value.value}' position : ${v.pos}").mkString
        
      case (e : Exception) => 
        error.textContent = "Exception : "+e.getMessage+" "+e.getClass

    


