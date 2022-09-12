package bon.jo
import bon.jo.Geom2D.*
import bon.jo.System.*
import bon.jo.pong.Shape
import Geom2D.Vector.*
package pong :
  enum Input:
    case Up,Down,Left,Right,No
  enum Gift(corner : Point, speed : Vector) extends Shape( ComputedPath(List(5*up,5*right,5*down,5*left),corner ),speed : Vector) :
    case NewBall(corner : Point,override val speed : Vector) extends Gift(corner : Point, speed : Vector)
    case Greater(corner : Point,override val speed : Vector) extends Gift(corner : Point, speed : Vector)
  object Gift:
    def random(pos : Point, speed : Vector):Gift = 
      if true then
        NewBall(pos,speed)
      else
        Greater(pos,speed)

  case class Board(paths : List[ComputedPath])extends Shape(paths.head,Vector(0,0)) with SystemElement:
    val seg = paths.flatMap(_.segments) 
    val minY = seg.map(s => Math.min(s.p1.y,s.p2.y)).min
    val maxY = seg.map(s => Math.max(s.p1.y,s.p2.y)).max
    val minX = seg.map(s => Math.min(s.p1.x,s.p2.x)).min
    val maxX = seg.map(s => Math.max(s.p1.x,s.p2.x)).max
    val h =  maxY - minY
    val w = maxX - minX
    def copy(pos: Point, speed:Vector): PosSpeed = copy(paths.map(_.copy(fromp = pos)))
    

  case class Ball(shape : DiscretCircle,speed : Vector) extends PosSpeed:
    def copy(pos: Point, speed: Vector):Ball = Ball(shape.copy(center = pos),speed)   
    def pos: Point = shape.center
  abstract class Shape(val valuep : ComputedPath,val speed : Vector) extends PosSpeed:

    def cross[C](s : Segment):(Debug,Drawer[C],C) ?=> Seq[Point] = valuep.segments.flatMap(_.cross(s))
    def pos: bon.jo.Geom2D.Point = valuep.from

  case class Rock(value : ComputedPath,override val speed : Vector = Vector(0,0),color : String,gift : Option[Gift]) extends Shape(value,speed):
    def copy(pos: Point, speed: Vector):Rock = Rock(value.copy(fromp = pos),speed,color,gift)
  case class Player(path : ComputedPath,override val speed : Vector)  extends Shape(path,speed):
    def copy(pos: Point, speed: Vector):Player = Player(path.copy(fromp = pos),speed)
  trait PosSpeed  extends SystemElement :
    def pos  : Point
    def speed  : Vector
    def copy(pos : Point,speed : Vector):PosSpeed
    def copy(pos : Point):PosSpeed = copy(pos,speed)
    def move[T <: PosSpeed]():T = 
      copy(pos + speed).asInstanceOf[T]

  case class PongSystem(balls : Seq[Ball],player : List[Player] ,board : Board,rocks : List[Rock],gifts : Seq[Gift] )extends System:
    def gameOver():Boolean = rocks.isEmpty || balls.isEmpty
    //def elements: Seq[SystemElement] = balls ++ player ++ rocks
  //object PongSystem:
    //def apply( l : List[SystemElement],board : Board  ) : PongSystem= 
      
     // PongSystem(l.head.asInstanceOf,l.tail.filter(_.isInstanceOf[Player]).asInstanceOf,board,l.tail.filter(_.isInstanceOf[Rock]).asInstanceOf)