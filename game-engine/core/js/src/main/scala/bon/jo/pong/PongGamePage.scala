package bon.jo.pong

import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import bon.jo.common.Geom2D.*
import bon.jo.common.Geom2D.Vector.*
import bon.jo.service.ScoreService
import bon.jo.pong.service.ScoreServiceRest
import bon.jo.service.SaveResult
import bon.jo.service.SaveResultSuccess
import bon.jo.html.request.BadStatusException
import bon.jo.domain.ScoreInfo
import bon.jo.pong
import org.scalajs.dom.document
import org.scalajs.dom.window
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import scalajs.js.special.debugger
import scala.concurrent.ExecutionContext.Implicits.global
import bon.jo.html.HttpServiceConfig.AuthParam.given
import scala.util.{Success, Failure}
import bon.jo.html.HtmlSplashMessage
import bon.jo.common.SideEffect.Serveur
import org.scalajs.dom.AudioNode
import org.scalajs.dom.HTMLAudioElement
import org.scalajs.dom.TouchEvent
import bon.jo.domain.UserContext
import org.scalajs.dom.Event
import StateMove.*
import org.scalajs.dom.KeyboardEvent

import DrawerCanvas.*
import DrawerCanvas.given
import SysBuilder.*
import bon.jo.pong.PongGamePage.given
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.HTMLLabelElement
import org.scalajs.dom.HTMLInputElement
import scalajs.js.JSConverters.*
import org.scalajs.dom.HTMLButtonElement
import scala.scalajs.js.JSON
object PongGamePage :

  
  given PointCount = PointCount(pointsByRock = 50,pointsByGift = 75, pointsBySecond = 10)
  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39
 

  def currentMillis = System.currentTimeMillis()
  
  def format(milli: Long): String =
    val ml = milli % 1000
    val s_t = milli / 1000
    val m = s_t / 60
    val s = s_t % 60
    f"${m}%02dm${s}%02ds${ml}%03dms"
  def go(using UserContext,ScoreService, Serveur[String]): Unit =
    HtmlSplashMessage(text = "Play",goAfter,"Yes").show()
  def goAfter(using UserContext,ScoreService, Serveur[String]): Unit = PongGamePage( createSys(3)).play()
    
  

class PongGamePage(createMySys : => PongSystem)(using UserContext,ScoreService, Serveur[String]):
  var u: PongSystem = createMySys
  var currentInterval: Option[Int] = None
  val audio = <.audio[HTMLAudioElement]
  val topSCoreWrapper : HTMLElement = <.div[HTMLElement](text("Score"),_class("dialog height-main"))
  val board = u.board
  val canvas = <.canvas[HTMLCanvasElement](_class("canvas-g")) > (_.height =
    (board.h).toInt, _.width = board.w.toInt )
  val timeDiv = <.div[HTMLElement] { text("0s") }
  val scoreDiv = <.div[HTMLElement](text("0"), _class("score"))
  val athDiv = <.div[HTMLElement] {
    childs(timeDiv, scoreDiv); _class("dialog")
  }  
  val leftPad : Ref[HTMLElement] = Ref()
  val rightPad:  Ref[HTMLElement] = Ref()
  val pad = div(_class("pad"),childs(
      div(_class("dir-pad  x-sym"),childs(image(src("./assets/img/right_arrow.svg"))),bind(leftPad)),
      div(_class("dir-pad"),childs(image(src("./assets/img/right_arrow.svg"))),bind(rightPad))
    ))
  
  val root = <.div[HTMLElement](
    childs(topSCoreWrapper,
      <.div[HTMLElement](childs(athDiv, canvas,pad), _class("ath-game"))
    ),
    _class("root")
  )
  var t = PongGamePage.currentMillis
  var startMove = End
 

  given Debug = () => debugger()


  given CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  given ProcessPong[CanvasRenderingContext2D] =
    ProcessPong[CanvasRenderingContext2D]()
  given Drawer[CanvasRenderingContext2D] = DrawerCanvas


 





  lazy val gameEvents:Seq[EventKeeper[_]] =
  
    Seq(leftPad.value.eventKeeper[TouchEvent]("touchstart", e => {
      e.preventDefault() 
      leftPad.value.classList.add("dir-pad-press")
      startMove = Start(left) 
    }),
    rightPad.value.eventKeeper[TouchEvent]("touchstart", e => {
      e.preventDefault() 
      rightPad.value.classList.add("dir-pad-press")
      startMove = Start(right) 
    }),

    pad.eventKeeper[TouchEvent]("touchend", e => {
      e.preventDefault()
      rightPad.value.classList.remove("dir-pad-press")
      leftPad.value.classList.remove("dir-pad-press")
      startMove = End 
    }),
    pad.eventKeeper[TouchEvent]("touchcancel", e => {
      e.preventDefault()
      rightPad.value.classList.remove("dir-pad-press")
      leftPad.value.classList.remove("dir-pad-press")
      startMove = End 
    }),
    pad.eventKeeper[TouchEvent]("touchmove", e => {
      e.preventDefault()
    }),
    document.body.eventKeeper[KeyboardEvent]("keydown" , e => {
      if startMove == End then
        startMove = e.keyCode match
          case PongGamePage.leftK  => Start(left)
          case PongGamePage.rightK => Start(right)
          case _           => End
    }),
    document.body.eventKeeper[KeyboardEvent]("keyup" , e => { startMove = End }))


  val accPlayer = Player.AccSpped(1.5)
  val _10m = 1000*60*10
  def updateTopeScore() : Unit = 
    topSCoreWrapper.children.foreach(topSCoreWrapper.removeChild)
    TopScoreView.view.recover{
      case e => 
        e.printStackTrace()
        <.div[HTMLElement](text("Oups, problems with top score..."))
    }.foreach(topSCoreWrapper :+ _ )
 
  def speedChoose():ComputedPath = 
    val diago = 1*right + 1.5 * up
    val arrowLeftPart = Seq(0.5* left,4*up,0.5 * left,diago)
    var arrow = arrowLeftPart ++ arrowLeftPart.reverse.map(e => e.symY)
    arrow = arrow.map(v => -10 * v)
    val pathP = u.balls.head
    val middle = pathP.pos
    ComputedPath(arrow,middle)

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
      
  def editLvl():Unit =
    document.body :+ root
    u.draw()
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
      rockMap.clear()
      rockMap ++= JSON.parse(s).asInstanceOf[scalajs.js.Array[ExportRock]].toSeq.map(e => (Point(e.x,e.y),ExportRock.unapply(e))).toMap
      drawAll()
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
      <.label[HTMLLabelElement](text("delete")).>(_.htmlFor="delete"), input("radio"){bind(deleteRef)}.>(_.id="delete",_.name="action",_.value="delete"))
      )
    val lifRockHtml =  div(
      childs(<.label[HTMLLabelElement](text("1")).>(_.htmlFor="1"), input("radio"){bind(placeRef)}.>(_.id="1",_.name="life-rock",_.value="1",_.checked = true),
      <.label[HTMLLabelElement](text("2")).>(_.htmlFor="2"), input("radio"){bind(deleteRef)}.>(_.id="2",_.name="life-rock",_.value="2"),
      <.label[HTMLLabelElement](text("3")).>(_.htmlFor="3"), input("radio"){bind(deleteRef)}.>(_.id="3",_.name="life-rock",_.value="3"))
    )
    val out =  div
    val saveDiv =  div(
      childs(button(text("save"),click(_ => out.textContent = rockJsString())),out))
    val restoreDiv =  div(text("paste here for restore")).>(_.contentEditable="true")
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
      val point = Point(e.clientX - bound.x,e.clientY- bound.y)
      val x1 =( (point.x/board.w*gridX).toInt)*board.w/gridX
      val y1 = (( point.y/board.h*gridY).toInt)*board.h/gridY
      val onGridPoint = Point(x1,y1)
      currentAction match
        case Action.place => rockMap.getOrElseUpdate(onGridPoint,Rock(SysBuilder.rockPath(board.w/gridX,board.h/gridY,onGridPoint),Vector(0,0),"green",None,currentLifRock))
        case Action.delete => rockMap -= onGridPoint
      drawAll()


    canvas.onmousedown  = e =>  mouseDown = true
    canvas.onmouseup  = e =>  mouseDown = false
    canvas.onclick = doAction
    canvas.onmousemove  = e => 
      if mouseDown then
       doAction(e)
  def play(): Unit =  
    document.body :+ root
    audio.src = "./assets/sound/lunarosa.wav"
    audio.load();   
    audio.play();  
    updateTopeScore()
    u.draw()
    var arrowDir = speedChoose()
    arrowDir.draw()
    ctx.fillStyle = "white"
    arrowDir.fill()
    lazy val ek : EventKeeper[KeyboardEvent] = document.body.eventKeeper[KeyboardEvent]("keydown",_ =>{
      val speedDir = arrowDir.elementsp.slice(0,arrowDir.elementsp.length/2).fold(Vector(0,0))( _ + _)
      ek.removeEventListener()
      window.clearInterval(int)
      u = u.copy(balls = u.balls.map(e => e.copy(speed = e.speed.length * speedDir.unitary() )))
      play_()
    })
    lazy val int: Int = window.setInterval(
      () => {
        u.draw()
        arrowDir = arrowDir.copy( arrowDir.elementsp.map(_.rotate(0.1)))
        arrowDir.draw()
        ctx.fillStyle = "white"
        arrowDir.fill()
      }
      ,50)
    ek.addEventListener()
    int

  private def play_(): Unit =  
    u.draw()
    gameEvents.foreach(_.addEventListener())
    var count = 0
    
    t = PongGamePage.currentMillis
    lazy val int: Int = window.setInterval(
      () => {
        startMove match
          case Start(dirPlayer) =>
            startMove = AddedEffect
            u = u.copy(player = u.player.map(_.copy(dir = dirPlayer)))
            u = u.copy(player = u.player.map { p =>
              p.addEffect(accPlayer)
            })
          case End =>
            u = u.copy(player = u.player.map { p =>
              p.copy(
                speed = Vector(0, 0),
                speedPlayer = 1,
                effects = p.effects.filter(_ != accPlayer)
              )
            })
          case AddedEffect =>

        // for(i <- 1 to 15)
        u = u.nextSystem()
        count += 1

        val timeLeft = _10m - PongGamePage.currentMillis + t
        timeDiv.textContent = PongGamePage.format(timeLeft)
        scoreDiv.textContent = u.player.head.baseScore.toString()
        if count % 10 == 0 then
          count = 0

          val nBalls = u.balls.map { b =>
            val nv =
              if b.speed.y >= 0 && b.speed.y < 1 then
                b.speed.rotate(Math.PI / 4)
              else if b.speed.y < 0 && b.speed.y > -1 then
                b.speed.rotate(-Math.PI / 4)
              else b.speed
            b.withPosAndSpeed(b.pos, nv)

          }
          u = u.copy(balls = nBalls)
        u.isGameOver(timeLeft) match
          case e: pong.End      => end(e)
          case pong.GameOver.No =>

        u.draw()

      },
      1000 / 20
    )
    currentInterval = Some(int)
  def replay():Unit = { u =createMySys; play() }
  def end(e: pong.End) =
    window.clearInterval(currentInterval.get)
    currentInterval = None
    val (scoreDivDetails,efftcts) = DetailsScoreView(e)
    val splash = HtmlSplashMessage(replay(),"yes")
    splash.dialog :+ scoreDivDetails
    scoreDiv.textContent = e.score.toString()
    splash.show()
    efftcts.start()
    summon[ScoreService].saveScore(ScoreInfo(1, 1, e.score)).onComplete {
      case Success(v) =>
        val messageS = v match
          case SaveResultSuccess(ok)   => 
            updateTopeScore() 
            splash addText "You update your Score !"
          case _ =>  splash addText "Not your best score !"
        splash addText "Play again?"
        

      case Failure(v) =>
        v match
          case BadStatusException(msg) =>
            splash addText ("Oups ...","Can't send best score","Play again?")

    }




  
    
def message(ok: => Unit, messages: String*): HTMLElement =
  val splash = HtmlSplashMessage(ok, "ok")
  messages.map { str =>
                  <.div[HTMLElement] {
                    text(str)
                  }
  }.foreach(splash.dialog.:+)

  document.body.appendChild(splash.root)
  splash.root

enum StateMove:
  case Start(dir: Vector)
  case End
  case AddedEffect

extension (h : HTMLElement)
  def eventKeeper[T <: Event]( eType: String, l : scalajs.js.Function1[T,?]) = EventKeeper(h,eType,l)
class EventKeeper[T <: Event](val on: HTMLElement,val eType: String,val l : scalajs.js.Function1[T,?]):
  def addEventListener():EventKeeper[T] = 
    on.addEventListener[T](eType,l)
    this
  def removeEventListener():EventKeeper[T] = 
    on.removeEventListener[T](eType,l)
    this
