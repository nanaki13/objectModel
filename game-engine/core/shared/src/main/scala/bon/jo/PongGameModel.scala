package bon.jo
import bon.jo.Geom2D.*

package pong :
  enum Input:
    case Up,Down,Left,Right,No
  case class Board(paths : List[ComputedPath])extends SystemElement:
    val h = {
      val seg = paths.flatMap(_.segments)
      val min = seg.map(s => Math.min(s.p1.y,s.p2.y)).min
      val max = seg.map(s => Math.max(s.p1.y,s.p2.y)).max
      max - min
    }
    val w = {
      val seg = paths.flatMap(_.segments)
      val min = seg.map(s => Math.min(s.p1.x,s.p2.x)).min
      val max = seg.map(s => Math.max(s.p1.x,s.p2.x)).max
      max - min
    }

  case class Ball(pos : Point,speed : Vector) extends PosSpeed:
    def copy(pos: Point, speed: Vector):Ball = Ball(pos,speed)   
  abstract class Shape(val valuep : ComputedPath,speed : Vector) extends PosSpeed:
    def cross[C](s : Segment):(Debug,Drawer[C],C) ?=> List[Point] = valuep.segments.flatMap(_.cross(s))
    def pos: bon.jo.Geom2D.Point = valuep.from

  case class Rock(value : ComputedPath,speed : Vector = Vector(0,0)) extends Shape(value,speed):
    def copy(pos: Point, speed: Vector):Rock = Rock(value.copy(from = pos),speed)
  case class Player(path : ComputedPath,speed : Vector)  extends Shape(path,speed):
    def copy(pos: Point, speed: Vector):Player = Player(path.copy(from = pos),speed)
  trait PosSpeed  extends SystemElement :
    def pos  : Point
    def speed  : Vector
    def copy(pos : Point,speed : Vector):PosSpeed
    def copy(pos : Point):PosSpeed = copy(pos,speed)
    def move[T <: PosSpeed]():T = copy(pos + speed).asInstanceOf

  case class PongSystem(ball : Ball,player : List[Player] ,board : Board,rocks : List[Rock] )extends System:
    def elements: List[SystemElement] = (ball :: player) ++ rocks
  object PongSystem:
    def apply( l : List[SystemElement],board : Board  ) : PongSystem= 
      
      PongSystem(l.head.asInstanceOf,l.tail.filter(_.isInstanceOf[Player]).asInstanceOf,board,l.tail.filter(_.isInstanceOf[Rock]).asInstanceOf)