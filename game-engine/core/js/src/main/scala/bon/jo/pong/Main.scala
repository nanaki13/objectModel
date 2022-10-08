package bon.jo.pong

import bon.jo.System.*
import bon.jo.pong
import bon.jo.pong.ProcessPong
import bon.jo.pong.ProcessPong.giftRandom
import bon.jo.Geom2D.*
import bon.jo.html.Html.*
import bon.jo.Geom2D.Vector.*
import org.scalajs.dom.document
import org.scalajs.dom.console
import org.scalajs.dom.window
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.CanvasRenderingContext2D
import scalajs.js.special.debugger
import bon.jo.pong.Debug
import scala.util.Random
import org.scalajs.dom.HTMLImageElement
import concurrent.ExecutionContext.Implicits.global
import bon.jo.service.ScoreService
import scala.concurrent.Future
import bon.jo.pong.service.ScoreServiceRest
import bon.jo.service.ScoreService.SaveResult
import bon.jo.domain.ScoreInfo
import scala.util.Success
import scala.util.Failure
import bon.jo.request.BadStatusException

object Main extends Drawer[CanvasRenderingContext2D]:

  def clear(board: Board): CanvasDraw[Unit] =
    ctx.clearRect(0, 0, board.w * 2, board.h * 2)

  def circle(pos: Point, r: Double): CanvasDraw[Unit] =

    ctx.ellipse(
      x = pos.x,
      y = pos.y,
      radiusX = r,
      radiusY = r,
      rotation = 0,
      startAngle = 0,
      endAngle = 2 * Math.PI
    )
    ctx.fill()

  extension (s: Segment)
    def draw(): CanvasDraw[Unit] =
      ctx.beginPath()
      ctx.moveTo(s.p1.x, s.p1.y)
      ctx.lineTo(s.p2.x, s.p2.y)
      ctx.stroke()
  extension (s: Path)
    def draw(): CanvasDraw[Unit] =

      ctx.moveTo(s.from.x, s.from.y)
      var pc = s.from
      for (p <- s.elements)
        pc = pc + p
        ctx.lineTo(pc.x, pc.y)
      ctx.fill()
  // val ballImage = <.img[HTMLImageElement].>(_.src="https://img.lovepik.com/element/45007/2796.png_300.png")

  
  extension (ball: Ball)
    def draw(): CanvasDraw[Unit] =
      val pos = ball.pos
      val gradient = ctx.createRadialGradient(
        pos.x,
        pos.y,
        ball.shape.r / 10,
        pos.x,
        pos.y,
        ball.shape.r
      );
      val r = ball.shape.r
// Add three color stops
      gradient.addColorStop(0, "pink")
      gradient.addColorStop(0.5, "gold")
      gradient.addColorStop(1, "red")

// Set the fill style and draw a rectangle
      ctx.beginPath()
      ctx.fillStyle = gradient
      // ctx.fillRect(pos.x-r,pos.y-r,2*r,2*r)
      circle(pos, ball.shape.r)
      val fs = ctx.fillStyle
      ctx.fillStyle = "red"
      // ctx.drawImage(ballImage,pos.x-ball.shape.r,pos.y-ball.shape.r,2*ball.shape.r,2*ball.shape.r)
      ctx.fillStyle = fs

  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39
  def randomHSL() =
    val r = Random()
    s"hsl(${r.nextDouble * 360}deg ${r.nextDouble * 100}% ${r.nextDouble * 100}%)"
  case class SysParam(fact: Int)
  inline def sysParam: SysParam ?=> SysParam = summon
  inline def fact: SysParam ?=> Int = sysParam.fact
  def makeRocks(
      width: Double,
      height: Double,
      blockByRow: Int,
      blockByColumn: Int,
      from: Point
  ): Seq[Rock] =
    val widthBlock = width / blockByRow
    val heightBlock = height / blockByColumn
    val fromV = Vector(from)
    for {
      xi <- 0 until blockByRow
      yi <- 0 until blockByColumn
      p = Point(widthBlock * xi, heightBlock * yi)
      r = Rock(
        ComputedPath(
          List(
            heightBlock * up,
            widthBlock * right,
            heightBlock * down,
            widthBlock * left
          ),
          p + fromV
        ),
        Vector(0, 0),
        randomHSL(),
        None
      )
    } yield r
  inline def makeRocks(
      board: Board,
      blockByRow: Int,
      blockByColumn: Int
  ): SysParam ?=> Seq[Rock] =
    val minw = board.paths
      .flatMap(_.segments.flatMap(z => List(z.p1, z.p2)))
      .map(_.x)
      .min
    makeRocks(
      board.w - 10 * fact,
      board.h * 0.5,
      blockByRow,
      blockByColumn,
      Point(minw + 5 * fact, board.h * 0.1)
    )

  def createSys(fact: Int) =
    given SysParam = SysParam(fact)
    val board = Board(
      List(
        ComputedPath(
          List(
            10 * fact * left,
            200 * fact * down,
            150 * fact * right,
            200 * fact * up,
            10 * fact * left,
            5 * fact * up,
            15 * fact * right,
            210 * fact * down,
            160 * fact * left,
            210 * fact * up,
            15 * fact * right,
            5 * fact * down
          ),
          Point(15 * fact, 205 * fact)
        )
      )
    )
    val teta = Math.PI / 8
    val path = for {
      i <- 1 until 8
      d = i * teta

    } yield 4 * fact * (down rotate d)

    // ++ List(25* fact * right,5* fact * down,25* fact * left,5* fact * up)
    val base =
      3 * fact * down +: path.map(v => Vector(v.x, v.y / 2)) :+ 3 * fact * up
    val tot = base.reduce(_ + _)
    val pathPlayer = ComputedPath(
      base :+ (-tot),
      Point(75 * fact, 203 * fact)
    ) // .biso(2* fact)

    def r1 = Random.nextInt(10) * 12 * fact
    def r2 = Random.nextInt(10) * 20 * fact
    def point = Point(10 + r1, 50 + r2)
    val rocks =
      // for(_ <- 0 until 50 ) yield Rock(ComputedPath(List(5 * fact* up,10 * fact* right,5* fact * down,10* fact * left),point).biso(2* fact))
      makeRocks(board, 7, 15).map(_.giftRandom)
    PongSystem(
      Ball(
        DiscretCircle(3 * fact, Point(board.w / 2, board.h - 10 * 10), 8),
        Vector(5, -5),
        Seq()
      ) :: Nil,
      Player(pathPlayer, Vector(0, 0), 0.2, 3 * fact, Vector(0, 0)) :: Nil,
      board,
      rocks.toList,
      Seq.empty
    )

  given DoDraw[CanvasRenderingContext2D, Gift] with
    def apply(board: Gift): CanvasRenderingContext2D ?=> Unit =
      gradient(ctx, board)
      ctx.beginPath()
      board.valuep.draw()
      board.drawDef()
  given DoDraw[CanvasRenderingContext2D, Board] with
    def apply(board: Board): CanvasRenderingContext2D ?=> Unit =
      gradient(ctx, board)
      board.paths.foreach { p =>
        ctx.beginPath()
        p.draw()

      }
      board.drawDef()
  given DoDraw[CanvasRenderingContext2D, Rock] with

    def apply(s: Rock): CanvasRenderingContext2D ?=> Unit =
      ctx.beginPath()

      val pO = Point(s.value.from.x, s.value.from.y)
      ctx.moveTo(s.value.from.x, s.value.from.y)
      val pfin = s.value.elements.foldLeft(pO)((pp, v) => {
        val p = pp + v
        ctx.lineTo(p.x, p.y)
        p
      })
      val o = ctx.fillStyle
      ctx.fillStyle = s.color
      gradient(ctx, s)
      ctx.fill()
      ctx.fillStyle = o
      s.drawDef()
  def gradient(ctx: CanvasRenderingContext2D, r: Shape): Unit =
    val gr = ctx.createLinearGradient(
      r.pos.x,
      r.pos.y,
      r.pos.x + r.valuep.w,
      r.pos.y + r.valuep.h
    )
    gr.addColorStop(0.1, "#96379d")
    gr.addColorStop(0.5, "pink")
    r match
      case e: Rock => gr.addColorStop(0.7, e.color)
      case _       => gr.addColorStop(0.7, "#96379d")

    ctx.fillStyle = gr
  given DoDraw[CanvasRenderingContext2D, Player] with
    def apply(r: Player): CanvasRenderingContext2D ?=> Unit =
      ctx.beginPath()
      val gr =
        ctx.createLinearGradient(r.pos.x, r.pos.y, r.pos.x + r.path.w, r.pos.y)
      gr.addColorStop(0.1, "#96379d")
      gr.addColorStop(0.2, "pink")
      gr.addColorStop(0.4, "#96379d")
      val o = ctx.fillStyle
      ctx.fillStyle = gr
      r.path.draw()
      ctx.fillStyle = o
      r.drawDef()
  def currentMillis = System.currentTimeMillis()
  var currentInterval: Option[Int] = None
  @main
  def launch(): Unit =
    Login.log().foreach(f => go(using f))
  def go(using Login.UserContext): Unit =
    val fact = 3
    // println(pseudo)
    given scoreService: ScoreService = ScoreServiceRest()
    val topSCoreWrapper : HTMLElement = <.div[HTMLElement](text("Score"),_class("dialog"))
    def updateTopeScore() : Unit = 
      topSCoreWrapper.children.foreach(topSCoreWrapper.removeChild)
      TopScoreView.view.recover{
        case e => 
          e.printStackTrace()
          println("Oups, problems with top score...")
          <.div[HTMLElement](text("Oups, problems with top score..."))
      }.foreach(topSCoreWrapper :+ _ )
    updateTopeScore()
    given Debug = () => debugger()

    var u: PongSystem = createSys(fact)
    val board = u.board
    val canvas = <.canvas[HTMLCanvasElement] > (_.height =
      (board.h).toInt, _.width = board.w.toInt)
    val timeDiv = <.div[HTMLElement] { text("0s") }
    val scoreDiv = <.div[HTMLElement](text("0"), _class("score"))
    val athDiv = <.div[HTMLElement] {
      childs(timeDiv, scoreDiv); _class("dialog")
    }
    def onLogOut() =
      currentInterval.foreach(window.clearInterval(_))

    val root = <.div[HTMLElement](
      childs(
        Login.logoutButton(onLogOut()),topSCoreWrapper,
        <.div[HTMLElement](childs(athDiv, canvas), _class("ath-game"))
      ),
      _class("root")
    )
    var t = currentMillis
    given CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    given ProcessPong[CanvasRenderingContext2D] =
      ProcessPong[CanvasRenderingContext2D]()
    given Drawer[CanvasRenderingContext2D] = this
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
          case Main.leftK  => Start(left)
          case Main.rightK => Start(right)
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
          def format(milli: Long): String =
            val ml = milli % 1000
            val s_t = milli / 1000
            val m = s_t / 60
            val s = s_t % 60
            f"${m}%02dm${s}%02ds${ml}%03dms"
            
          timeDiv.textContent = format(_10m - currentMillis + t)
          scoreDiv.textContent = u.player.head.score.toString()
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
          u.isGameOver() match
            case e: pong.End      => end(e)
            case pong.GameOver.No =>

          u.draw()

        },
        1000 / 20
      )

      def end(e: pong.End) =
        window.clearInterval(int)
        currentInterval = None
        scoreService.saveScore(ScoreInfo(1, 1, e.score)).onComplete {
          case Success(v) =>
            val messageS = v match
              case SaveResult.Updated    => 
                updateTopeScore() 
                "You update your Score !"
              case SaveResult.NotUpdated => "Not your best score !"
            message({ u = createSys(fact); play() }, messageS, "Play again?")

          case Failure(v) =>
            v match
              case BadStatusException(msg) =>
                message({ u = createSys(fact); play() }, "Oups ...","Can't send best score","Play again?")

        }

      currentInterval = Some(int)

    play()

    def message(ok: => Unit, messages: String*): Unit =
      lazy val cont: HTMLElement = <.div[HTMLElement] {
        _class("splash")
        childs(
          <.div[HTMLElement] {
            _class("flex-center")
            childs(
              <.div[HTMLElement] {
                _class("dialog")
                childs(
                  <.div[HTMLElement] {
                    childs(messages.map { str =>
                      <.div[HTMLElement] {
                        text(str)
                      }
                    }*)
                  },
                  <.div[HTMLElement] {
                    childs(
                      <.button[HTMLElement](text("yes")).>(
                        _.onclick = _ => {
                          ok
                          document.body.removeChild(cont)
                        }
                      )
                    )
                  }
                )
              }
            )
          }
        )
      }

      document.body.appendChild(cont)
