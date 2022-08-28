package bon.jo

import bon.jo.pong.PongSystem
import bon.jo.pong.Ball
import bon.jo.pong.PosSpeed
import bon.jo.pong.Player
import bon.jo.Geom2D.*
import bon.jo.pong.Board
class ProcessPong(using List[SystemElement] => PongSystem) extends SystemElementProcess[PongSystem]:

    def next(s: SystemElement): ElementFlow[PongSystem] = 
      val board : Board = summon.board
      //println(s.asInstanceOf[Ball].pos)
      s match
        case e : PosSpeed  =>  
          val segs = board.paths.flatMap(_.segments)
          val seg = Segment(e.pos, e.pos + e.speed)
          val crosss = segs.map{
            sB => sB.cross(seg)
          }
          println(crosss)
          val nBall = e.copy(pos = e.pos + e.speed)
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
          nBall.copy(Point(x,y ),Vector(vx,vy ) )
        case _ =>  s