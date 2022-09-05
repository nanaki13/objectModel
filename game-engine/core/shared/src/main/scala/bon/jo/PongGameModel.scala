package bon.jo
import bon.jo.Geom2D.*
import bon.jo.System.*
package pong :
  enum Input:
    case Up,Down,Left,Right,No
  enum Gift(pos : Point, speed : Vector) extends PosSpeed:
    case NewBall(pos : Point, speed : Vector) extends Gift(pos : Point, speed : Vector)
  case class Board(paths : List[ComputedPath])extends SystemElement:
    val seg = paths.flatMap(_.segments) 
    val minY = seg.map(s => Math.min(s.p1.y,s.p2.y)).min
    val maxY = seg.map(s => Math.max(s.p1.y,s.p2.y)).max
    val minX = seg.map(s => Math.min(s.p1.x,s.p2.x)).min
    val maxX = seg.map(s => Math.max(s.p1.x,s.p2.x)).max
    val h =  maxY - minY
    val w = maxX - minX

  case class Ball(shape : DiscretCircle,speed : Vector) extends PosSpeed:
    def copy(pos: Point, speed: Vector):Ball = Ball(shape.copy(center = pos),speed)   
    def pos: Point = shape.center
  abstract class Shape(val valuep : ComputedPath,speed : Vector) extends PosSpeed:
    def cross[C](s : Segment):(Debug,Drawer[C],C) ?=> List[Point] = valuep.segments.flatMap(_.cross(s))
    def pos: bon.jo.Geom2D.Point = valuep.from

  case class Rock(value : ComputedPath,speed : Vector = Vector(0,0),color : String) extends Shape(value,speed):
    def copy(pos: Point, speed: Vector):Rock = Rock(value.copy(from = pos),speed,color)
  case class Player(path : ComputedPath,speed : Vector)  extends Shape(path,speed):
    def copy(pos: Point, speed: Vector):Player = Player(path.copy(from = pos),speed)
  trait PosSpeed  extends SystemElement :
    def pos  : Point
    def speed  : Vector
    def copy(pos : Point,speed : Vector):PosSpeed
    def copy(pos : Point):PosSpeed = copy(pos,speed)
    def move[T <: PosSpeed]():T = copy(pos + speed).asInstanceOf

  case class PongSystem(balls : Seq[Ball],player : List[Player] ,board : Board,rocks : List[Rock],gifts : Seq[Gift] )extends System:
    def gameOver():Boolean = rocks.isEmpty || balls.isEmpty
    //def elements: Seq[SystemElement] = balls ++ player ++ rocks
  //object PongSystem:
    //def apply( l : List[SystemElement],board : Board  ) : PongSystem= 
      
     // PongSystem(l.head.asInstanceOf,l.tail.filter(_.isInstanceOf[Player]).asInstanceOf,board,l.tail.filter(_.isInstanceOf[Rock]).asInstanceOf)