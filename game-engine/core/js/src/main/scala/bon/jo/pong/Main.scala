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

  def circle(pos : Point, r : Double): CanvasDraw[Unit] = 
    ctx.beginPath()
    ctx.ellipse(x = pos.x,y = pos.y,radiusX = r,radiusY = r,rotation = 0,startAngle = 0,endAngle = 2*Math.PI)
    ctx.fill()
  extension (s : Segment)
    def draw() :CanvasDraw[Unit] = 
      ctx.beginPath()
      ctx.moveTo(s.p1.x,s.p1.y)
      ctx.lineTo(s.p2.x,s.p2.y)
      ctx.stroke()
      
  extension (ball : Ball)
    def draw() :CanvasDraw[Unit] = 
      val pos = ball.pos
      
      circle(pos,ball.shape.r)
      val fs = ctx.fillStyle
      ctx.fillStyle="red"
      ball.shape.points().foreach(circle(_ , 0.5))
      ctx.fillStyle=fs
  extension (player : Player)
    def draw() :CanvasDraw[Unit] = 
      player.path.draw()
  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39 
  @main
  def test2():Unit =
    val fact  = 3
    val board = Board(List(
      (ComputedPath(List(50 * fact * left,300* fact * down,50 * fact* right),Point(50* fact,300* fact)) join
      (ComputedPath(List(50* fact * right,300* fact * down,50* fact * left),Point(100* fact,300* fact)).reverse())).biso(4* fact)
    ))
    val pathPlayer =  ComputedPath(List(25* fact * right,5* fact * down,25* fact * left,5* fact * up),Point(75* fact,298* fact)).biso(2* fact)
    def r1 = +Random.nextInt(10)*12* fact
    def r2 = Random.nextInt(10)*20* fact
    def point = Point(10+r1,50+r2)
    val rocks =  for(_ <- 0 until 50 ) yield Rock(ComputedPath(List(5 * fact* up,10 * fact* right,5* fact * down,10* fact * left),point).biso(2* fact))
    def createSys() = PongSystem(Ball(DiscretCircle(3* fact, Point(board.w / 2,board.h -10*10 ),8),10*Vector(1,-2.3)),Player(pathPlayer,Vector(0,0))::Nil,  board,rocks.toList)
    var u : PongSystem = createSys()
    given (List[SystemElement] => PongSystem) =  PongSystem.apply(_,u.board)
    given Debug = () =>  debugger()
    
    //console.log(<.canvas )
    val canvas  = <.canvas[HTMLCanvasElement]> (_.height = (board.h*1.2).toInt,_.width = (board.w*1.2).toInt)
    val root = <.div[HTMLElement](childs(<.div[HTMLElement](childs(canvas),style(_.margin ="auto",_.width ="fit-content"))),style(_.width ="100%",_.marginTop ="4%")) 
    given CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    given Drawer[CanvasRenderingContext2D] = this
    given  SystemProcess[PongSystem]  = ProcessPong[CanvasRenderingContext2D]()

    document.body :+ root
    var dir = 0
    document.body.onkeydown = e => dir = e.keyCode
    document.body.onkeyup= e => dir = 0
    u = u.copy(rocks = Nil)
    play()
    def play():Unit = 
      u.draw()
      lazy val int : Int = window.setInterval(() =>{
        val speedPlayer = dir match
          case Main.leftK => 5 * fact * left
          case Main.rightK => 5 * fact  * right
          case _ => Vector(0,0)
        
        u = u.copy(player = u.player.map{
          p => p.copy(p.pos,speedPlayer)

        })
        //for(i <- 1 to 10)
        u = u.nextSystem()
        
        if u.gameOver() then
          
          window.clearInterval(int)
          lazy val cont : HTMLElement = <.div[HTMLElement]{
            _class("splash")
            childs(
              <.div[HTMLElement]{
                _class("flex-center")
                childs(
                  <.div[HTMLElement]{
                    text("play again ?")
                  },
                  <.div[HTMLElement]{
                    childs((<.button[HTMLElement](text("yes")).>(
                      _.onclick = _ => {
                          u = createSys()
                          play()
                          document.body.removeChild(cont)
                        })))
                  }
                )
              }
            )  
          }
          document.body.appendChild(cont)
          

        u.draw()
      
      },1000/25 )
      int

     
    
  
   
    

