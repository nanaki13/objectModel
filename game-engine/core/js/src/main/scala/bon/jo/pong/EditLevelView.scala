package bon.jo.pong
import bon.jo.common.Geom2D.*
import org.scalajs.dom.document
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLLabelElement
import org.scalajs.dom.HTMLButtonElement
import scala.scalajs.js.JSON
import scala.scalajs.js.JSConverters.*
import org.scalajs.dom.MouseEvent
import DrawerCanvas.*
import DrawerCanvas.given
import scala.concurrent.Future
import bon.jo.domain.UserContext
import bon.jo.html.HttpServiceConfig.AuthParam.given
import concurrent.ExecutionContext.Implicits.global
enum Action:
  case place,delete
trait ExportRock extends scalajs.js.Object:
  val life : Int
  val x:Double
  val y:Double
  val w:Double
  val h:Double
object ExportRock:
  def apply(r : Rock):ExportRock = 
    scalajs.js.Dynamic.literal(
      life = r.life,
      x = r.pos.x,
      y = r.pos.y,
      w = r.value.w,
      h = r.value.h
    ).asInstanceOf[ExportRock]
  def unapply(r : ExportRock):Rock = Rock(SysBuilder.rockPath(r.w,r.h,Point(r.x,r.y)),Vector(0,0),"green",None,r.life)
      
trait EditLevelView:
  self : PongGamePage =>
    def editLvl():UserContext ?=>Unit =
      document.body :+ root
      u.draw()
      document.body
      var gridX = 20
      var gridY = 40
      var currentAction = Action.place
      var currentLifRock = 1
      val placeRef = Ref[HTMLInputElement]()
      val deleteRef = Ref[HTMLInputElement]()
    
      val gridXInput = Ref[HTMLInputElement]()
      val gridYInput = Ref[HTMLInputElement]()
      val saveButton = Ref[HTMLButtonElement]()
      val rockMap = scala.collection.mutable.Map[Point,Rock]()
      def rockJs(): scalajs.js.Array[ExportRock] = 
        rockMap.values.map(ExportRock.apply).toJSArray
      def rockJsString() = scalajs.js.JSON.stringify(rockJs())
      def restore(s : String):Unit = 
        (if s.startsWith("/") then
          PongGamePage.rockFromUrl(s)
        else
          Future.successful{
            JSON.parse(s).asInstanceOf[scalajs.js.Array[ExportRock]].toSeq.map(ExportRock.unapply(_))
          }) map {
            ro => 
              rockMap.clear()
              rockMap ++= ro.map(e => (e.pos,e)).toMap
              drawAll()

          }

      def drawAll():Unit=
        DrawerCanvas.clear(board)
        board.draw()
        rockMap.values.foreach(_.draw())

      val gridControl = div(
        childs(<.label[HTMLLabelElement](text("gridX")).>(_.htmlFor="gx"), input("text"){bind(gridXInput)}.>(_.id="gx",_.value=gridX.toString,_.onchange = _ => gridX = gridXInput.value.value.toInt ),
        <.label[HTMLLabelElement](text("gridY")).>(_.htmlFor="gy"), input("text"){bind(gridYInput)}.>(_.id="gy",_.value=gridY.toString,_.onchange = _ => gridY = gridYInput.value.value.toInt))
        )
      val controlsHtml = div(
        childs(<.label[HTMLLabelElement](text("place")).>(_.htmlFor="place"), input("radio"){bind(placeRef)}.>(_.id="place",_.name="action",_.value="place",_.checked = true),
        <.label[HTMLLabelElement](text("delete")).>(_.htmlFor="delete"), input("radio"){bind(deleteRef)}.>(_.id="delete",_.name="action",_.value="delete")
        , button(text("Delete All"),click(_ => {
            rockMap.clear()
            drawAll()
          }))
        )
        )
      val lifRockHtml =  div(
        childs(<.label[HTMLLabelElement](text("1")).>(_.htmlFor="1"), input("radio"){bind(placeRef)}.>(_.id="1",_.name="life-rock",_.value="1",_.checked = true),
        <.label[HTMLLabelElement](text("2")).>(_.htmlFor="2"), input("radio"){bind(deleteRef)}.>(_.id="2",_.name="life-rock",_.value="2"),
        <.label[HTMLLabelElement](text("3")).>(_.htmlFor="3"), input("radio"){bind(deleteRef)}.>(_.id="3",_.name="life-rock",_.value="3"))
      )
      val out =  div(_class("_20-10-ov-auto"))
      val saveDiv =  div(
        childs(button(text("save"),click(_ => out.textContent = rockJsString())),out))
      val restoreDiv =  div(_class("_20-10-ov-auto"),text("paste here for restore")).>(_.contentEditable="true")
      val srestoreDiv =  div(
        childs(restoreDiv,button(text("restore"),click(_ => restore( restoreDiv.textContent) ))))
      
      val allControls = div(childs(gridControl,controlsHtml,lifRockHtml,saveDiv,srestoreDiv))
    

    
      controlsHtml.getElementsByTagName("input").map(_.asInstanceOf[HTMLInputElement]).foreach{
      inp => inp.onchange = e => 
        currentAction = Action.valueOf(inp.value)
      }
      lifRockHtml.getElementsByTagName("input").map(_.asInstanceOf[HTMLInputElement]).foreach{
      inp => inp.onchange = e => 
        currentLifRock = inp.value.toInt
      }
      
      root :+ allControls
      var mouseDown = false
      
      val doAction  = (e: MouseEvent) => 
        val bound = canvas.getBoundingClientRect()
        val point = Point(e.clientX - bound.x - board.padding,e.clientY- bound.y- board.padding)
        val x1 =( (point.x/board.wp*gridX).toInt)*board.wp/gridX + board.padding
        val y1 = (( point.y/board.hp*gridY).toInt)*board.hp/gridY + board.padding
        val onGridPoint = Point(x1,y1)
        currentAction match
          case Action.place => rockMap.getOrElseUpdate(onGridPoint,Rock(SysBuilder.rockPath(board.wp/gridX,board.hp/gridY,onGridPoint),Vector(0,0),"green",None,currentLifRock))
          case Action.delete => rockMap -= onGridPoint
        drawAll()


      canvas.onmousedown  = e =>  mouseDown = true
      canvas.onmouseup  = e =>  mouseDown = false
      canvas.onclick = doAction
      canvas.onmousemove  = e => 
        if mouseDown then
        doAction(e)
  
