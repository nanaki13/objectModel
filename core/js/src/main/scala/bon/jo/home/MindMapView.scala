package bon.jo.home
import bon.jo.HtmlPredef.*
import bon.jo.MiniDsl.*
import bon.jo.objects.All.Dsl.{obj,emptyObj}
import bon.jo.objects.All
import bon.jo.objects.All.ObjectProp
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLDivElement
import concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.HTMLButtonElement
object MindMapView:

  class Context(var data : All[String],var path : All.Path[String],val out : HTMLElement,val global : HTMLElement)
  val storage = org.scalajs.dom.window.localStorage
  type CtxUnit = Context ?=> Unit
  type Ctx[A] = Context ?=> A
  inline def ctx(using Context):Context = summon
  inline def data: Ctx[All[String]]=
    if ctx.path.values.isEmpty then ctx.data else ctx.data( ctx.path)
  def goto(p : String,prop : All[String]): CtxUnit = 
    org.scalajs.dom.console.log(prop)
    ctx.path = ctx.path / p  
    view(prop)
  def goParent(): CtxUnit = 

    ctx.path = All.Path(ctx.path.values.dropRight(1))

   
    if ctx.path.values.isEmpty then
      view(ctx.data)
    else
      view(ctx.data(ctx.path))
  def pathView(): Ctx[HTMLDivElement] = 
     div(childs(
        ctx.path.paths.flatMap(
        pth => 
          
          List(span(_text("/"))
            ,a(_text(pth.values.last),click{ e => 
              e.preventDefault()
              ctx.path = pth
              view(ctx.data(ctx.path))
            },me(_.href="") )
          
          )
        
        ).toList
      ))

  def saveData(data : All[String]):Unit = 
    storage.setItem("mindmap",data.toJsonString())
  def resetCurrent(): CtxUnit =
    updateData( ctx.data.delete(ctx.path))
    view(All.Empty())
  def readData():Option[All[String]] = 
    Option(storage.getItem("mindmap")).map(All.apply)
  def view():HTMLElement = 
    val data : All[String] = readData().getOrElse(emptyObj())
    
    given Context = Context(data,All.Path(Nil),div,div)
    view(data)
    div(childs(ctx.out,ctx.global))

  def saveData(): CtxUnit = 
    ctx.global.innerText = ctx.data.toJsonString()
    saveData(ctx.data)

  def updateData(p : All[String]): CtxUnit = 
    ctx.data = ctx.data.update(ctx.path,p)
    println("update : ")
    println(ctx.data)
    saveData()

    
  def resetView(): Ctx[HTMLButtonElement] = button(_text("change type"),click(resetCurrent()))
  def backView(): Ctx[HTMLButtonElement] = button(_text("parent"),click(goParent()))
  def view(p :  All[String]) :CtxUnit = 
    ctx.out.innerHTML= ""
    ctx.out.append(pathView())
    if(ctx.path.values.nonEmpty) then
      ctx.out.append(backView())
    ctx.out.append(resetView())
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
              val buttonSub = button(_text("OK"),  click{
              val newValue : All[String] =  All.Value(inp.value)  
              updateData(newValue)
              view(newValue)
              })
              ctx.out.appendChild(buttonSub)
              
              
            case "D" => 
              val newValue : All[String] =  All.Dsl.emptyObj()  
              updateData(newValue)
              view(newValue)
          
            

        
      case e : All.ObjectAll[String] =>
        objectView(e)
      case All.Value(v : String) =>
         valueView(All.Value(v))

      case _ => 

    def addProp(prp : String,objView : HTMLElement) = 
      val inputp = input(_class("small-input"))
      val seeButton = a(_text("go to"),me(_.href=""))
      var current = prp.toString
      seeButton.onclick = e => 
        e.preventDefault()
        goto(current,ctx.data(ctx.path/current))
      inputp.value = prp.toString
      if !ctx.data(ctx.path).contains( prp.toString) then
         ctx.data = ctx.data.empty(ctx.path / prp.toString)
         saveData()
 
    
      inputp.onchange = ev => 
        org.scalajs.dom.console.log(ev)
        println(ctx.data)
        val nData = data.replace(inputp.value,current.toString)
        updateData(nData)      
        current = inputp.value


        
      objView.append(inputp,seeButton)
    def valueView(e : All.Value[String,String]):CtxUnit =
     
      val in = div(me(_.contentEditable = "true"),me(_.innerHTML = e.value))
      val saveButton = button(_text("save"),click{
        updateData(All.Value(in.innerHTML))  
       })
      ctx.out.append(in,saveButton)
     

    def objectView(e : All.ObjectAll[String]):CtxUnit =
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

      StepAsk.SimpleStepAsk(addButton,input,button(_text("ok"))){
        propName =>
          addProp(propName,objView)
        
      }
      clearButton.onclick = _ =>   
        
        objView.innerHTML = ""  
        updateData(emptyObj())


      e.props.foreach{
        prp => 
          println("call addProp")
          println(prp)
          addProp(prp.key,objView)
      }
      

      

