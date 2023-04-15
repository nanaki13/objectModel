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
import scalajs.js
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
import scala.concurrent.Future
import bon.jo.html.request.HttpRequest.GlobalParam
import bon.jo.html.request.HttpRequest.given
import bon.jo.pong.Login.given
import bon.jo.html.request.HttpRequest.*
import bon.jo.html.request.HttpRequest.Method.*
import bon.jo.common.typeutils.~
import bon.jo.html.request.predef.*
import PongGamePage.Levelsinfo
import PongGamePage.levelsInfo
import PongGamePage.lvl
import PongGamePage.gameInfo
import bon.jo.common.SideEffect
object PongGamePage :

  trait Levelsinfo extends js.Object:
    val lvlCount : Int
  val basePath = "/break-broke/assets/js"
  given PointCount = PointCount(pointsByRock = 50,pointsByGift = 75, pointsBySecond = 10)
  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39
  given Conversion[ js.Any, Levelsinfo] = e =>  e.asInstanceOf[Levelsinfo]
  def fethLevelsInfo():GlobalParam ?=> Future[Levelsinfo] =  GET.send(s"$basePath/levels-info.json",None,Map.empty[String,String]).map(_.okWithJs(200))
  def withLevelsInfo(f : Levelsinfo ?=> Unit ):GlobalParam ?=>Future[Unit] = 
      fethLevelsInfo().map{
        case given Levelsinfo => f
      }
  inline def levelsInfo : ~[Levelsinfo] = summon
  def currentMillis = System.currentTimeMillis()

  given Conversion[ js.Any, Rock] = e =>  ExportRock.unapply(e.asInstanceOf[ExportRock])
  def rockFromUrl(str : String):GlobalParam ?=> Future[Seq[Rock]] = 

    GET.send(str,None,Map.empty[String,String]).map(_.okWithJs(200))

  
  
  def format(milli: Long): String =
    val ml = milli % 1000
    val s_t = milli / 1000
    val m = s_t / 60
    val s = s_t % 60
    f"${m}%02dm${s}%02ds${ml}%03dms"
  def go(using UserContext,ScoreService, Serveur[String],Levelsinfo): Unit =
    HtmlSplashMessage(text = "Play",goAfter,"Yes").show()

  def goAfter(using UserContext,ScoreService, Serveur[String],Levelsinfo): Unit = 
    given GameInfo = GameInfo(lvl = 1)
   

    PongGamePage(pngSystem).play(1)
      
  def pngSystem(lvl : Int): UserContext ?=>  Future[PongSystem] = rockFromUrl(s"$basePath/lvl-$lvl.json").map(createSys(3,_))
   
  def lvl :GameInfo ?=> Int = summon.lvl
  def gameInfo : ~[GameInfo] = summon
 
  def lvl_=(l : Int) :GameInfo ?=> Unit = summon.lvl = l

case class GameInfo( var lvl :Int = 1)

  
class PongGameElement(
      var u : PongSystem,
      val canvas : HTMLCanvasElement ):
    val board = u.board
    canvas.height = board.h.toInt
    canvas.width = board.w.toInt 

class PongGamePage(createMySys : (lvl : Int) => Future[PongSystem])(using UserContext,ScoreService, Serveur[String],Levelsinfo,GameInfo):
  inline def el: PongGameElement ?=> PongGameElement =  summon
  inline def u: PongGameElement ?=> PongSystem =  summon.u
 
  //var u: PongSystem = createMySys(lvl)
  var currentInterval: Option[Int] = None
  val audio = <.audio[HTMLAudioElement]
  val topSCoreWrapper : HTMLElement = div(_class("dialog height-main"))
 // val board = u.board
  val canvas = <.canvas[HTMLCanvasElement](_class("canvas-g"))
  val timeDiv = div { text("0s") }
  val scoreDiv = div(text("0"), _class("score"))
  val lvlDiv = div
  val athDiv = div {
    childs(lvlDiv,timeDiv, scoreDiv); _class("dialog")
  }  



  val leftPad : Ref[HTMLElement] = Ref()
  val rightPad:  Ref[HTMLElement] = Ref()
  val pad = div(_class("pad"),childs(
      div(_class("dir-pad  x-sym"),childs(image(src("./assets/img/right_arrow.svg"))),bind(leftPad)),
      div(_class("dir-pad"),childs(image(src("./assets/img/right_arrow.svg"))),bind(rightPad))
    ))
  
  val root = div(
    childs(topSCoreWrapper,
      div(childs(athDiv, canvas,pad), _class("ath-game"))
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
    TopScoreView.view(lvl).recover{
      case e => 
        e.printStackTrace()
        div(text("Oups, problems with top score..."))
    }.foreach(topSCoreWrapper :+ _ )
 
  def speedChoose():PongGameElement ?=> ComputedPath = 
    val diago = 1*right + 1.5 * up
    val arrowLeftPart = Seq(0.5* left,4*up,0.5 * left,diago)
    var arrow = arrowLeftPart ++ arrowLeftPart.reverse.map(e => e.symY)
    arrow = arrow.map(v => -10 * v)
    val pathP = u.balls.head
    val middle = pathP.pos
    ComputedPath(arrow,middle)

  def play(lvl : Int):Unit = 
    createMySys(lvl).map(play)

  def play(p : PongSystem): Unit =  
    given el : PongGameElement = new PongGameElement(p,canvas)
    //println(SideEffect.serveur[String].emit("test"))
    //println(SideEffect.serveur[GameInfo])
    println("ddddddd")

    lvlDiv.textContent = s"LVL ${gameInfo.lvl}"
    topSCoreWrapper.textContent = s"Score - LVL$lvl"

    document.body :+ root
    audio.src = "./assets/sound/lunarosa.wav"
    audio.load();   
    audio.play();  
    updateTopeScore()
    p.draw()
    var arrowDir = speedChoose()
    arrowDir.draw()
    ctx.fillStyle = "white"
    arrowDir.fill()
    lazy val ek : EventKeeper[KeyboardEvent] = document.body.eventKeeper[KeyboardEvent]("keydown",_ =>{
      val speedDir = arrowDir.elementsp.slice(0,arrowDir.elementsp.length/2).fold(Vector(0,0))( _ + _)
      ek.removeEventListener()
      window.clearInterval(int)
       el.u =  el.u.copy(balls = el.u.balls.map(e => e.copy(speed = e.speed.length * speedDir.unitary() )))
      play_()
    })
    lazy val int: Int = window.setInterval(
      () => {
        el.u.draw()
        arrowDir = arrowDir.copy( arrowDir.elementsp.map(_.rotate(0.1)))
        arrowDir.draw()
        ctx.fillStyle = "white"
        arrowDir.fill()
      }
      ,20)
    ek.addEventListener()
    int


  private def play_(): PongGameElement ?=> Unit =  
    el.u.draw()
    gameEvents.foreach(_.addEventListener())
    var count = 0
    
    t = PongGamePage.currentMillis
    lazy val int: Int = window.setInterval(
      () => {
        startMove match
          case Start(dirPlayer) =>
            startMove = AddedEffect
             el.u =  el.u.copy(player = u.player.map(_.copy(dir = dirPlayer)))
             el.u =  el.u.copy(player = u.player.map { p =>
              p.addEffect(accPlayer)
            })
          case End =>
             el.u =  el.u.copy(player = u.player.map { p =>
              p.copy(
                speed = Vector(0, 0),
                speedPlayer = 1,
                effects = p.effects.filter(_ != accPlayer)
              )
            })
          case AddedEffect =>

        // for(i <- 1 to 15)
        el.u =  el.u.nextSystem()
        count += 1

        val timeLeft = _10m - PongGamePage.currentMillis + t
        timeDiv.textContent = PongGamePage.format(timeLeft)
        scoreDiv.textContent =  el.u.player.head.baseScore.toString()
        if count % 10 == 0 then
          count = 0

          val nBalls =  el.u.balls.map { b =>
            val nv =
              if b.speed.y >= 0 && b.speed.y < 1 then
                b.speed.rotate(Math.PI / 4)
              else if b.speed.y < 0 && b.speed.y > -1 then
                b.speed.rotate(-Math.PI / 4)
              else b.speed
            b.withPosAndSpeed(b.pos, nv)

          }
          el.u =  el.u.copy(balls = nBalls)
        el.u.isGameOver(timeLeft) match
          case e: pong.End      => end(e)
          case pong.GameOver.No =>

        el.u.draw()

      },
      1000 / 20
    )
    currentInterval = Some(int)
  def replay():Unit = 
    createMySys(lvl).onComplete{
      
        case Failure(exception) => exception.printStackTrace()
        case Success(value) => play(value)
      
    }
  def end(e: pong.End) =
    window.clearInterval(currentInterval.get)
    currentInterval = None
    val (scoreDivDetails,efftcts) = DetailsScoreView(e)

    
    val splash = HtmlSplashMessage(replay(),"yes")
    splash.dialog :+ scoreDivDetails
    scoreDiv.textContent = e.score.toString()
    splash.show()
    efftcts.start()
    summon[ScoreService].saveScore(ScoreInfo(1, lvl, e.score)).onComplete {
      case Success(v) =>
        val messageS = v match
          case SaveResultSuccess(ok)   => 
            updateTopeScore() 
            splash addText "You update your Score !"
          case _ =>  splash addText "Not your best score !"
          
        e match
          case _ : GameOver.Victory if levelsInfo.lvlCount > lvl => 
              lvl = lvl + 1
              splash addText s"Next level : $lvl?"
          case _ : GameOver.Victory => 
              lvl  = 1
              splash addText s"YOU FINISH THE GAME !!"
              splash addText "Play again?"
          case _ : GameOver.Loose =>
             splash addText "GameOver"
             splash addText "Play again?"
        
       
        

      case Failure(v) =>
        v match
          case BadStatusException(msg) =>
            splash addText ("Oups ...","Can't send best score","Play again?")

    }




  
    
def message(ok: => Unit, messages: String*): HTMLElement =
  val splash = HtmlSplashMessage(ok, "ok")
  messages.map { str =>
                  div {
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
