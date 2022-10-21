package bon.jo.pong

import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import bon.jo.Geom2D.*
import bon.jo.Geom2D.Vector.*
import bon.jo.service.ScoreService
import bon.jo.pong.service.ScoreServiceRest
import bon.jo.service.SaveResult
import bon.jo.service.SaveResultSuccess
import bon.jo.request.BadStatusException
import bon.jo.domain.ScoreInfo
import bon.jo.pong
import org.scalajs.dom.document
import org.scalajs.dom.window
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import scalajs.js.special.debugger
import scala.concurrent.ExecutionContext.Implicits.global
import bon.jo.pong.HttpServiceConfig.AuthParam.given
import scala.util.{Success, Failure}
import bon.jo.html.HtmlSplashMessage
import bon.jo.common.SideEffect.Serveur
import org.scalajs.dom.AudioNode
import org.scalajs.dom.HTMLAudioElement
object PongGamePage :

  import DrawerCanvas.*
  import DrawerCanvas.given
  import SysBuilder.*
  
  given PointCount = PointCount(pointsByRock = 50,pointsByGift = 75, pointsBySecond = 10)
  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39
 

  def currentMillis = System.currentTimeMillis()
  var currentInterval: Option[Int] = None
  def format(milli: Long): String =
    val ml = milli % 1000
    val s_t = milli / 1000
    val m = s_t / 60
    val s = s_t % 60
    f"${m}%02dm${s}%02ds${ml}%03dms"
  def go(using Login.UserContext, Serveur[String]): Unit =
    HtmlSplashMessage(tex = "Play",goAfter).show()
  def goAfter(using Login.UserContext, Serveur[String]): Unit =
    
    val audio = <.audio[HTMLAudioElement]
    audio.src = "./assets/sound/lunarosa.wav"
    audio.load();   
    audio.play();   
    val fact = 3
    // println(pseudo)
    given scoreService: ScoreService = ScoreServiceRest()
    val topSCoreWrapper : HTMLElement = <.div[HTMLElement](text("Score"),_class("dialog"))
    def updateTopeScore() : Unit = 
      topSCoreWrapper.children.foreach(topSCoreWrapper.removeChild)
      TopScoreView.view.recover{
        case e => 
          e.printStackTrace()
          <.div[HTMLElement](text("Oups, problems with top score..."))
      }.foreach(topSCoreWrapper :+ _ )
    updateTopeScore()
    given Debug = () => debugger()

    var u: PongSystem = createSys(fact)
    val board = u.board
    val canvas = <.canvas[HTMLCanvasElement](_class("canvas-g")) > (_.height =
      (board.h).toInt, _.width = board.w.toInt )
    val timeDiv = <.div[HTMLElement] { text("0s") }
    val scoreDiv = <.div[HTMLElement](text("0"), _class("score"))
    val athDiv = <.div[HTMLElement] {
      childs(timeDiv, scoreDiv); _class("dialog")
    }


    val root = <.div[HTMLElement](
      childs(topSCoreWrapper,
        <.div[HTMLElement](childs(athDiv, canvas), _class("ath-game"))
      ),
      _class("root")
    )
    var t = currentMillis
    given CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    given ProcessPong[CanvasRenderingContext2D] =
      ProcessPong[CanvasRenderingContext2D]()
    given Drawer[CanvasRenderingContext2D] = DrawerCanvas
    enum StateMove:
      case Start(dir: Vector)
      case End
      case AddedEffect
    import StateMove.*

    document.body :+ root

    var startMove = End

    document.body.onkeydown = e => {
      if startMove == End then
        startMove = e.keyCode match
          case PongGamePage.leftK  => Start(left)
          case PongGamePage.rightK => Start(right)
          case _           => End
    }
    document.body.onkeyup = e => { startMove = End }
    // u = u.copy(rocks = Nil)

    val accPlayer = Player.AccSpped(1.5)
    val _10m = 1000*60*10
    def play(): Unit =
      u.draw()
      var count = 0
      t = currentMillis
      lazy val int: Int = window.setInterval(
        () => {
          startMove match
            case Start(dirPlayer) =>
              startMove = AddedEffect
              println("Start move")
              u = u.copy(player = u.player.map(_.copy(dir = dirPlayer)))
              u = u.copy(player = u.player.map { p =>
                p.withEffect(p.effects :+ accPlayer)
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

          val timeLeft = _10m - currentMillis + t
          timeDiv.textContent = format(timeLeft)
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
          // println(currentMillis - t)
          u.isGameOver(timeLeft) match
            case e: pong.End      => end(e)
            case pong.GameOver.No =>

          u.draw()

        },
        1000 / 20
      )
      def replay():Unit = { u = createSys(fact); play() }
      def end(e: pong.End) =
        window.clearInterval(int)
        currentInterval = None
        val (scoreDivDetails,efftcts) = DetailsScoreView(e)
        val splash = HtmlSplashMessage(replay(),"yes")
        splash.dialog :+ scoreDivDetails
        scoreDiv.textContent = e.score.toString()
        splash.show()
        efftcts.start()
        scoreService.saveScore(ScoreInfo(1, 1, e.score)).onComplete {
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

      currentInterval = Some(int)

    play()
 
    
  def message(ok: => Unit, messages: String*): HTMLElement =
    val splash = HtmlSplashMessage(ok, "ok")
    messages.map { str =>
                    <.div[HTMLElement] {
                      text(str)
                    }
    }.foreach(splash.dialog.:+)

    document.body.appendChild(splash.root)
    splash.root
