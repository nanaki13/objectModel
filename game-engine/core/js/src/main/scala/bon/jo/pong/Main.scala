package bon.jo.pong

import bon.jo.System.*
import bon.jo.pong.ProcessPong
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

object Main extends Drawer[CanvasRenderingContext2D] :
  
  def clear(board: Board): CanvasDraw[Unit] =
    ctx.clearRect(0,0,board.w*2,board.h*2)



  extension (s : Segment)
    def draw() :CanvasDraw[Unit] = 
      ctx.beginPath()
      ctx.moveTo(s.p1.x,s.p1.y)
      ctx.lineTo(s.p2.x,s.p2.y)
      ctx.stroke()
      
  extension (ball : Ball)
    def draw() :CanvasDraw[Unit] = 
      val pos = ball.pos
      ctx.beginPath()
      ctx.ellipse(x = pos.x,y = pos.y,radiusX = ball.shape.r,radiusY = ball.shape.r,rotation = 0,startAngle = 0,endAngle = 2*Math.PI)
      ctx.fill()
  extension (player : Player)
    def draw() :CanvasDraw[Unit] = 
      player.path.draw()
  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39 
  @main
  def test2():Unit =
    
    val board = Board(List(
      (ComputedPath(List(50 * left,300 * down,50 * right),Point(50,300)) join
      (ComputedPath(List(50 * right,300 * down,50 * left),Point(100,300)).reverse())).biso(4)
    ))
    val pathPlayer =  ComputedPath(List(25 * right,5 * down,25 * left,5 * up),Point(75,298)).biso(2)
    def r1 = +Random.nextInt(10)*12
    def r2 = Random.nextInt(10)*20
    def point = Point(10+r1,50+r2)
    val rocks =  for(_ <- 0 until 50 ) yield Rock(ComputedPath(List(5 * up,10 * right,5 * down,10 * left),point).biso(2))
    var u : PongSystem = PongSystem(Ball(DiscretCircle(1.5, Point(10,board.h -10 ),8),2*Vector(1,-2.3)),Player(pathPlayer,Vector(0,0))::Nil,  board,rocks.toList)
    given (List[SystemElement] => PongSystem) =  PongSystem.apply(_,u.board)
    given Debug = () =>  debugger()
    
    //console.log(<.canvas )
    val canvas  = <.canvas[HTMLCanvasElement]> (_.height = (board.h*1.2).toInt,_.width = (board.w*1.2).toInt)
    val root = <.div[HTMLElement](childs(<.div[HTMLElement](childs(canvas),style(_.margin ="auto",_.width ="fit-content"))),style(_.width ="100%",_.marginTop ="10%")) 
    given CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    given Drawer[CanvasRenderingContext2D] = this
    given  SystemProcess[PongSystem]  = ProcessPong[CanvasRenderingContext2D]()

    document.body :+ root
    var dir = 0
    document.body.onkeydown = e => dir = e.keyCode
    document.body.onkeyup= e => dir = 0
    u.draw()
    window.setInterval(() =>{
      val speedPlayer = dir match
        case Main.leftK => 5 * left
        case Main.rightK => 5 * right
        case _ => Vector(0,0)
      
      u = u.copy(player = u.player.map{
        p => p.copy(p.pos,speedPlayer)

      })
      u = u.nextSystem()
      u.draw()
    
     },1000/25 )
     
    
  
   
    

