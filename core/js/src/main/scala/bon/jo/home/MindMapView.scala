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
  inline def data: Ctx[All[String]]=
    if ctx.path.values.isEmpty then ctx.data else ctx.data( ctx.path)
  def goto(p : String,prop : All[String]): CtxUnit = 
    org.scalajs.dom.console.log(prop)
    ctx.path = ctx.path / p  
    view(prop)
  def back(): CtxUnit = 

    ctx.path = All.Path(ctx.path.values.dropRight(1))

   
    if ctx.path.values.isEmpty then
      view(ctx.data)
    else
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

  def updateData(p : All[String]): CtxUnit = 
    ctx.data = ctx.data.set(ctx.path,p)
    println("update : ")
    println(ctx.data)
    
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
              val buttonSub = button(click{
              val newValue : All[String] =  All.Value(inp.value)  
              updateData(newValue)
              view(newValue)
              })
              ctx.out.appendChild(buttonSub)
              
              
            case "D" => 
              val newValue : All[String] =  All.Dsl.emptyObj()  
              updateData(newValue)
              view(newValue)
          
            

        
      case e : All.Object[String] =>
        objectView(e)
      case All.Value(v : String) =>
         valueView(All.Value(v))

      case _ => 

    def addProp(prp : String,objView : HTMLElement) = 
      println("addProp = "+prp)
      println("addProp = "+ctx.data)
      val inputp = input(_class("small-input"))
      val seeButton = a(_text("go to"),me(_.href=""))
      var current = prp.toString
      seeButton.onclick = e => 
        e.preventDefault()
        println("ctx.data = "+ctx.data)
        println(ctx.path/current)
        println("seeee"+ctx.data(ctx.path/current))
        goto(current,ctx.data(ctx.path/current))
      inputp.value = prp.toString
      if !ctx.data(ctx.path).contains( prp.toString) then
         ctx.data = ctx.data.setEmpty(ctx.path / prp.toString)
 
    
      inputp.onchange = ev => 
        org.scalajs.dom.console.log(ev)
        ctx.data = ctx.data.replace(inputp.value,current.toString)
        org.scalajs.dom.console.log(ctx.data)
        current = inputp.value
      objView.append(inputp,seeButton)
    def valueView(e : All.Value[String,String]):CtxUnit =
     
      val in = div(me(_.contentEditable = "true"),me(_.innerHTML = e.v))
      val saveButton = button(_text("save"),click{
        updateData(All.Value(in.innerHTML))  
       })
      ctx.out.append(in,saveButton)
     

    def objectView(e : All.Object[String]):CtxUnit =
      println("ov : "+e)
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
        updateData(emptyObj())
      addButton.onclick = _ =>   
        addProp(count.toString,objView)
        count =( count.toShort + 1).toChar
      e.props.foreach{
        prp => 
          println("call addProp")
          println(prp)
          addProp(prp.key,objView)
      }
      

      

