package bon.jo.pong

import bon.jo.SystemElement
import bon.jo.SystemElementProcess
import bon.jo.ProcessPong
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
import bon.jo.Debug
import scala.util.Random

object Main extends Drawer[CanvasRenderingContext2D] :
  
  def clear(board: Board): CanvasDraw[Unit] =
    ctx.clearRect(0,0,board.w,board.h)



  extension (s : Segment)
    def draw() :CanvasDraw[Unit] = 
      ctx.beginPath()
      ctx.moveTo(s.p1.x,s.p1.y)
      ctx.lineTo(s.p2.x,s.p2.y)
      ctx.stroke()
  extension (ball : Ball)
    def draw() :CanvasDraw[Unit] = 
      val pos = ball.pos
      ctx.fillRect(pos.x-1,pos.y-1,2,2)
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
      ComputedPath(List(50 * up,300 * right,50 * down),Point(0,150)) join
      (ComputedPath(List(50 * down,300 * right,50 * up),Point(0,50)).reverse())
    ))
    val pathPlayer =  ComputedPath(List(25 * up,5 * right,25 * down,5 * left),Point(0,50))
    val rocks =  for(_ <- 0 to 100 ) yield Rock(ComputedPath(List(10 * up,5 * right,10 * down,5 * left),Point(100+Random.nextInt(200),50+Random.nextInt(100))))
    var u : PongSystem = PongSystem(Ball(Point(50,50),Vector(-4.2,2.3)),Player(pathPlayer,Vector(0,0))::Nil,  board,rocks.toList)
    given (List[SystemElement] => PongSystem) =  PongSystem.apply(_,u.board)
    given Debug = () =>  debugger()
    
    //console.log(<.canvas )
    val canvas  = <.canvas[HTMLCanvasElement]> (_.height = board.h.toInt,_.width = board.w.toInt)
    val root = <.div[HTMLElement](childs(<.div[HTMLElement](childs(canvas),style(_.margin ="auto",_.width ="fit-content"))),style(_.width ="100%",_.marginTop ="10%")) 
    given CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    given Drawer[CanvasRenderingContext2D] = this
    given  SystemElementProcess[PongSystem]  = ProcessPong[CanvasRenderingContext2D]()

    document.body :+ root
    var dir = 0
    document.body.onkeydown = e => dir = e.keyCode
    document.body.onkeyup= e => dir = 0
    u.draw()
    window.setInterval(() =>{
      val speedPlayer = dir match
        case Main.upK => 5 * down
        case Main.downK => 5 * up
        case _ => Vector(0,0)
      
      u = u.copy(player = u.player.map{
        p => p.copy(p.pos,speedPlayer)

      })
      u = u.nextSystem()
      u.draw()
    
     },1000/25 )
     
    
  
   
    

