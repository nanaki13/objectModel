package bon.jo

import bon.jo.pong.PongSystem
import bon.jo.pong.Ball
import bon.jo.pong.PosSpeed
import bon.jo.pong.Player
import bon.jo.Geom2D.*
import bon.jo.pong.Board
import bon.jo.pong.Drawer
trait Debug:
   def debug():Unit
   def debug(s  :String):Unit = 
      println(s)
      debug()
class ProcessPong[C](using List[SystemElement] => PongSystem,Debug,Drawer[C],C) extends SystemElementProcess[PongSystem]:

    def next(s: SystemElement): ElementFlow[PongSystem] = 
      val board : Board = summon.board
      //println(s.asInstanceOf[Ball].pos)
      s match
        case e : PosSpeed  =>  
          
          
         val seg = e.pos -> e.speed
         val nextDef = e.copy(seg.p2)
         e match
            case b : Ball =>
          
               val segs = board.paths.flatMap(_.segments) ++ summon.player.flatMap(_.path.segments) ++ summon.rocks.flatMap(_.value.segments)
               val crosss = segs.flatMap{
                  sB => 
                     sB.cross(seg).map{
                        i =>
                           val sym = seg.p2.sym(sB)
                           e.copy(pos = sym,speed = e.speed.length * Segment(i,sym).toVector().unitary())
                        
                     }
                  }
               crosss.headOption match
                  case Some(v) => v
                  case o => nextDef
            case _ => nextDef
            
         
            /*val nBall = e.copy(pos = e.pos + e.speed)
            val (x,vx) = if nBall.pos.x > board.w then
               val diff = nBall.pos.x - board.w
               (board.w - diff,-e.speed.x)
            else if nBall.pos.x < 0 then
               (- nBall.pos.x,-e.speed.x)
            else
               (nBall.pos.x,e.speed.x)
            val (y,vy) = if nBall.pos.y > board.h then
               val diff = nBall.pos.y - board.h
               (board.h - diff,-e.speed.y)
            else if nBall.pos.y < 0 then
               (- nBall.pos.y,-e.speed.y)
            else
               (nBall.pos.y,e.speed.y)
            nBall.copy(Point(x,y ),Vector(vx,vy ) )*/
        case _ =>  s