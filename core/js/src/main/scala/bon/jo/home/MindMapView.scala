package bon.jo.home
import bon.jo.HtmlPredef.*
import bon.jo.MiniDsl.*
import bon.jo.objects.All.Dsl.{obj,emptyObj}
import bon.jo.objects.All.Object
import bon.jo.objects.All
import bon.jo.objects.All.ObjectProp
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLDivElement
object MindMapView:

  class Context(var data : All[String],var path : All.Path[String],val out : HTMLElement)

  type CtxUnit = Context ?=> Unit
  type Ctx[A] = Context ?=> A
  inline def ctx(using Context):Context = summon
  def goto(p : String,prop : All[String]): CtxUnit = 
    org.scalajs.dom.console.log(prop)
    ctx.path = ctx.path / p  
    view(prop)
  def back(): CtxUnit = 
    ctx.path = All.Path(ctx.path.values.dropRight(1))
    view(ctx.data(ctx.path))
  def pathView(): Ctx[HTMLDivElement] = 
     div(childs(
        ctx.path.values.flatMap(pth => List(span(_text("/")),span(_text(pth))))
      ))
  def view():HTMLElement = 
    val data : All[String] = emptyObj()
    given Context = Context(data,All.Path(Nil),div)
    view(data)
    ctx.out

  def backView(): Ctx[HTMLDivElement] = div(_text("back"),click(back()))
  def view(p :  All[String]) :CtxUnit = 
    ctx.out.innerHTML= ""
    ctx.out.append(pathView())
    ctx.out.append(backView())
    p match
      case e : All.Empty[String] =>

        val selectp = select_(childs(option(_text("Text"),me(_.value="T")), option(_text("Dossier"),me(_.value="D"))))
        val submit = button(_text("create"))
        ctx.out.append(selectp,submit)
        submit.onclick = _ =>
          selectp.value match
            case "T" => 
              val inp = input
              ctx.out.appendChild(inp)
            case "D" => 
        
      case e : All.Object[String] =>
        objectView(e)

      case _ => 

    def addProp(prp : String,objView : HTMLElement) = 
      val inputp = input(_class("small-input"))
      val seeButton = a(_text("go to"),me(_.href=""))
      var current = prp.toString
      seeButton.onclick = e => 
        e.preventDefault()
        ctx.data.setEmpty(ctx.path/current)
        goto(current,All.Empty())
      inputp.value = prp.toString
      ctx.data = ctx.data.setEmpty(ctx.path / prp.toString)
      
      
      inputp.onchange = ev => 
        org.scalajs.dom.console.log(ev)
        ctx.data = ctx.data.replace(inputp.value,current.toString)
        org.scalajs.dom.console.log(ctx.data)
        current = inputp.value
      objView.append(inputp,seeButton)
    def objectView(e : All.Object[String]):CtxUnit =
      val addButton = button(_text("+"))

      val clearButton = button(_text("clear"))
      val objView = div
      val root = div(childs(
      
        addButton,
        clearButton,
        objView
      ))
      ctx.out.append(root)
      var count = 'a'
      clearButton.onclick = _ =>   
        count = 'a'
        objView.innerHTML = ""  
        ctx.data.set(ctx.path,emptyObj())
      addButton.onclick = _ =>   
        addProp(count.toString,objView)
        count =( count.toShort + 1).toChar
      e.props.foreach{
        prp => 

      }
      

      

