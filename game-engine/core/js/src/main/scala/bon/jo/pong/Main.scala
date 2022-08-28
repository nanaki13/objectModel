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
      val pos = player.pos
      ctx.fillRect(pos.x-1,pos.y-1,2,2)
     
  @main
  def test2():Unit =
    val board = Board(List(
      ComputedPath(List(50 * up,200 * right,50 * down),Point(0,150)),
      ComputedPath(List(50 * down,200 * right,50 * up),Point(0,50))
    ))
    var u : PongSystem = PongSystem(Ball(Point(50,50),Vector(4.2,2.3)),Player(Point(1,50),Vector(4.2,2.3))::Nil,  board)
    given (List[SystemElement] => PongSystem) =  PongSystem.apply(_,u.board)
    given  SystemElementProcess[PongSystem]  = ProcessPong()
    //console.log(<.canvas )
    val canvas  = <.canvas[HTMLCanvasElement]> (_.height = board.h.toInt,_.width = board.w.toInt)
    val root = <.div[HTMLElement](childs(<.div[HTMLElement](childs(canvas),style(_.margin ="auto",_.width ="fit-content"))),style(_.width ="100%")) 
    given CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val s1 = Segment(Point(100,70),Point(40,90))
    val s2 = Segment(Point(50,150),Point(50,200))
    val s3 = Segment(Point(160-15,70),Point(180-15,90))
    val s4 = Segment(Point(150,50),Point(150,100))
    println(up * right)
    println(up ^ right)
    println(up ^ left)
    def printTets(ab :Segment,cd :  Segment,name : String) = 
      println(name)
      val abV  = ab.toVector()
      val cdV  = cd.toVector()
      val ac =Vector(ab.p1,cd.p1)
      val ca = - ac
      val ad =Vector(ab.p1,cd.p2)
      val cb =Vector(cd.p1,ab.p2)
      val ab_v_cd = abV ^ cdV
      val ab_v_ad = abV ^ ad
      val ab_v_ac = abV ^ ac

      val cd_v_cb = cdV ^ cb
      val cd_v_ca = cdV ^ ca

      println("ab_v_cd = "+ab_v_cd)
      println("ab_v_ad * ab_v_ac = "+(ab_v_ad * ab_v_ac))
      println("cd_v_cb * cd_v_ca = "+(cd_v_cb * cd_v_ca))
      println(ab_v_cd != 0 && ab_v_ad * ab_v_ac <= 0 && cd_v_cb * cd_v_ca <= 0 )
     

    printTets(s1,s2,"s1 s2")
    printTets(s3,s4,"s3 s4")
    printTets(s4,s3,"s4 s3")
    printTets(s2,s4,"s2 s4")
    printTets(s1,s3,"s1 s3")
    document.body :+ root
  
    u.draw()
    window.setInterval(() =>{
      u = u.nextSystem()
      //println(u)
      u.draw()
      s1.draw()
      s2.draw()
      s3.draw()
      s4.draw()
    
     },1000/25 )
     
    
  
   
    

