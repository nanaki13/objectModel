package bon.jo
import bon.jo.Geom2D.*

package pong :
  case class Board(paths : List[ComputedPath]):
    val h = {
      val seg = paths.flatMap(_.segments)
      val min = seg.map(s => Math.min(s.p1.y,s.p2.y)).min
      val max = seg.map(s => Math.max(s.p1.y,s.p2.y)).max
      println((min,max))
      max - min
    }
    val w = {
      val seg = paths.flatMap(_.segments)
      val min = seg.map(s => Math.min(s.p1.x,s.p2.x)).min
      val max = seg.map(s => Math.max(s.p1.x,s.p2.x)).max
      max - min
    }

  case class Ball(pos : Point,speed : Vector) extends PosSpeed 

  case class Player(pos : Point,speed : Vector)  extends PosSpeed  
  trait PosSpeed  extends SystemElement :
    def pos  : Point
    def speed  : Vector
    def copy(pos : Point,speed : Vector):PosSpeed
    def copy(pos : Point):PosSpeed = copy(pos,speed)

  case class PongSystem(ball : Ball,player : List[Player] ,board : Board )extends System:
    def elements: List[SystemElement] = ball :: player
  object PongSystem:
    def apply( l : List[SystemElement],board : Board  ) : PongSystem= 
      
      PongSystem(l.head.asInstanceOf,l.tail.asInstanceOf,board)