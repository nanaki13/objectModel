package bon.jo.pong

import bon.jo.System.*
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

object Main extends Drawer[CanvasRenderingContext2D] :
  
  
  def clear(board: Board): CanvasDraw[Unit] =
    ctx.clearRect(0,0,board.w*2,board.h*2)

  def circle(pos : Point, r : Double): CanvasDraw[Unit] = 
    ctx.beginPath()
    ctx.ellipse(x = pos.x,y = pos.y,radiusX = r,radiusY = r,rotation = 0,startAngle = 0,endAngle = 2*Math.PI)
    ctx.fill()
  extension (s : Rock)
    def draw() :CanvasDraw[Unit] = 
      
      ctx.beginPath()
      val o = ctx.fillStyle
      ctx.fillStyle = s.color
      val pO = Point(s.value.from.x,s.value.from.y)
      val p1 = Point(s.value.from.x,s.value.from.y)
      ctx.moveTo(s.value.from.x,s.value.from.y)
      val pfin = s.value.elements.foldLeft(pO)( (pp,v)=> {
        val p = pp+v
        ctx.lineTo(p.x,p.y)
        p 
      })
      ctx.fill()
      ctx.fillStyle = o
      (s : Shape).draw()
  extension (s : Segment)
    def draw() :CanvasDraw[Unit] = 
      ctx.beginPath()
      ctx.moveTo(s.p1.x,s.p1.y)
      ctx.lineTo(s.p2.x,s.p2.y)
      ctx.stroke()
  val ballImage = <.img[HTMLImageElement].>(_.src="https://img.lovepik.com/element/45007/2796.png_300.png")

  extension (ball : Ball)
    def draw() :CanvasDraw[Unit] = 
      val pos = ball.pos
      
      //circle(pos,ball.shape.r)
      val fs = ctx.fillStyle
      ctx.fillStyle="red"
      ctx.drawImage(ballImage,pos.x-ball.shape.r,pos.y-ball.shape.r,2*ball.shape.r,2*ball.shape.r)
      ctx.fillStyle=fs
  extension (player : Player)
    def draw() :CanvasDraw[Unit] = 
      player.path.draw()
  val upK = 38
  val downK = 40
  val leftK = 37
  val rightK = 39 
  def randomHSL() = 
    val r = Random()
    s"hsl(${r.nextDouble*360}deg ${r.nextDouble*100}% ${r.nextDouble*100}%)"
  def makeRocks(width : Double,height : Double,blockByRow : Int,blockByColumn : Int,from : Point):Seq[Rock] = 
    val widthBlock = width / blockByRow
    val heightBlock = height / blockByColumn
    val fromV = Vector(from)
    for{
      xi <- 0 until blockByRow
      yi <- 0 until blockByColumn
      p = Point(widthBlock * xi,heightBlock * yi)
      r = Rock(ComputedPath(List(heightBlock* up,widthBlock* right,heightBlock * down,widthBlock * left),p+fromV),Vector(0,0),randomHSL(),None)
    } yield r
  inline def makeRocks(board : Board, blockByRow : Int,blockByColumn : Int):Seq[Rock] = 
    val minw = board.paths.flatMap(_.segments.flatMap(z => List(z.p1 ,z.p2))).map(_.x).min
    makeRocks(board.w,board.h*0.7,blockByRow,blockByColumn,Point(minw,board.h*0.1))  

  def createSys( fact :Int ) = 
    
    val board = Board(List(
      ComputedPath(List(10 * fact * left,300* fact * down,150 * fact* right,300* fact * up,10 * fact * left),Point(10* fact,300* fact)).biso(4* fact))
    )
    val pathPlayer =  ComputedPath(List(25* fact * right,5* fact * down,25* fact * left,5* fact * up),Point(75* fact,298* fact)).biso(2* fact)
    def r1 = +Random.nextInt(10)*12* fact
    def r2 = Random.nextInt(10)*20* fact
    def point = Point(10+r1,50+r2)
    val rocks =  
      //for(_ <- 0 until 50 ) yield Rock(ComputedPath(List(5 * fact* up,10 * fact* right,5* fact * down,10* fact * left),point).biso(2* fact))
      makeRocks(board,10,20).map(_.giftRandom)
    PongSystem(Ball(DiscretCircle(3* fact, Point(board.w / 2,board.h -10*10 ),8),Vector(0.5,-0.8))::Nil,Player(pathPlayer,Vector(0,0))::Nil,  board,rocks.toList,Seq.empty)
  @main
  def test2():Unit =

    val fact= 3
    
    
 
    given Debug = () =>  debugger()

    
    var u : PongSystem = createSys(fact)
    val board = u.board   
    val canvas  = <.canvas[HTMLCanvasElement]> (_.height = (board.h).toInt,_.width = (board.w*1.2).toInt)
    val root = <.div[HTMLElement](childs(<.div[HTMLElement](childs(canvas),style(_.margin ="auto",_.width ="fit-content"))),style(_.width ="100%",_.marginTop ="4%")) 
    given CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    given  ProcessPong[CanvasRenderingContext2D]  = ProcessPong[CanvasRenderingContext2D]()
    given Drawer[CanvasRenderingContext2D] = this


    document.body :+ root
    var dir = 0
    document.body.onkeydown = e => dir = e.keyCode
    document.body.onkeyup= e => dir = 0
    //u = u.copy(rocks = Nil)
    play()

    def currentMillis = System.currentTimeMillis()
    def play():Unit = 
      u.draw()
      lazy val int : Int = window.setInterval(() =>{
        val speedPlayer = dir match
          case Main.leftK => 0.5 * fact * left
          case Main.rightK => 0.5 * fact  * right
          case _ => Vector(0,0)
        
        u = u.copy(player = u.player.map{
          p => p.copy(p.pos,speedPlayer)

        })
        val t = currentMillis
        for(i <- 1 to 10)
          u = u.nextSystem()
        println(currentMillis - t)
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
                          u = createSys(fact)
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
      
      },1000/20 )
      int

     
    
  
   
    

