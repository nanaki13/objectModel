package bon.jo
import bon.jo.Geom2D.*
import bon.jo.System.*
import bon.jo.pong.Shape
import Geom2D.Vector.*
package pong :
  type WithEffs[T] = T & Effects[T]
  trait Effects[T]:
    
    me : T  =>
      val effects : Seq[Effect[T]] 
      type EffOnMe = Effect[T]
      def withEffect(effs : Seq[EffOnMe]):WithEffs[T]
      def applyEffect():T = 
        
        if this.effects.nonEmpty then
          val res : (WithEffs[T],Seq[EffOnMe]) = this.effects.foldLeft(this -> Seq.empty[EffOnMe])( (acc,eff) => {
            val (t,effs) = acc
            val (neff,nt) = eff(t)
            nt -> (effs:+neff)
          } )
          val (nMe,nEff) = res
          nMe.withEffect(nEff.filterNot(_.isFinish))
        else
          this
  trait Effect[T]:
    val count:Int
    def effect(t : WithEffs[T]):WithEffs[T]
    def countDown():Effect[T]

    def apply(t : WithEffs[T]):(Effect[T],WithEffs[T]) = 
       countDown() -> effect(t)

    def isFinish:Boolean = count <= 0
  object Effect:
    def apply[T](count : Int,  effectf : T &Effects[T]=> WithEffs[T]): Effect[T] = Impl(count,effectf)
    case class Impl[T ](count : Int,  effectf : T &Effects[T]=> WithEffs[T]) extends Effect[T]:
      inline def effect(t : WithEffs[T]):WithEffs[T] = effectf(t)
      def countDown():Effect[T] = copy(count-1)
  enum Input:
    case Up,Down,Left,Right,No
  enum Gift(corner : Point, speed : Vector) extends Shape( ComputedPath(List(5*up,5*right,5*down,5*left),corner ),speed : Vector) :
    case NewBall(corner : Point,override val speed : Vector) extends Gift(corner : Point, speed : Vector)
    case GreaterPlayer(corner : Point,override val speed : Vector) extends Gift(corner : Point, speed : Vector)
    case GreaterBall(corner : Point,override val speed : Vector) extends Gift(corner : Point, speed : Vector)
    case SmallerBall(corner : Point,override val speed : Vector) extends Gift(corner : Point, speed : Vector)
  object Gift:
    def random(pos : Point, speed : Vector):Gift = 
      val r = Math.random()
      if  r < 1d/4 then
        NewBall(pos,speed)
      else if r < 2d/4  then
        GreaterPlayer(pos,speed)
      else if r < 3d/4 then
        GreaterBall(pos,speed)
      else
        SmallerBall(pos,speed)

  case class Board(paths : List[ComputedPath])extends Shape(paths.head,Vector(0,0)) with SystemElement:
    val seg = paths.flatMap(_.segments) 
    val minY = seg.map(s => Math.min(s.p1.y,s.p2.y)).min
    val maxY = seg.map(s => Math.max(s.p1.y,s.p2.y)).max
    val minX = seg.map(s => Math.min(s.p1.x,s.p2.x)).min
    val maxX = seg.map(s => Math.max(s.p1.x,s.p2.x)).max
    val h =  maxY - minY
    val w = maxX - minX
    def copy(pos: Point, speed:Vector): PosSpeed = copy(paths.map(_.copy(fromp = pos)))
    

  case class Ball(shape : DiscretCircle,speed : Vector, effects : Seq[Effect[Ball]]) extends PosSpeed with Effects[Ball] :


    def withEffect(effs: Seq[Effect[Ball]]): Ball = copy(effects = effs)
    def copy(pos: Point, speed: Vector):Ball = copy(shape = shape.copy(center = pos),speed=speed,effects=effects)   
    def pos: Point = shape.center
  object Ball:
    def multSizeEffect(count : Int,fact : Double):Effect[Ball] = 
      Effect(count,me =>
        me.copy(shape = me.shape.copy( r = me.shape.r * fact)) 
      )
  abstract class Shape(val valuep : ComputedPath,val speed : Vector) extends PosSpeed:

    def cross[C](s : Segment):(Debug,Drawer[C],C) ?=> Seq[Point] = valuep.segments.flatMap(_.cross(s))
    def pos: bon.jo.Geom2D.Point = valuep.from

  case class Rock(value : ComputedPath,override val speed : Vector = Vector(0,0),color : String,gift : Option[Gift]) extends Shape(value,speed):
    def copy(pos: Point, speed: Vector):Rock = Rock(value.copy(fromp = pos),speed,color,gift)
  case class Player(path : ComputedPath,override val speed : Vector, effects : Seq[Effect[Player]] = Seq.empty)  extends Shape(path,speed) with Effects[Player] :
    def copy(pos: Point, speed: Vector):Player = Player(path.copy(fromp = pos),speed,effects)
    def withEffect(effs: Seq[Effect[Player]]): Player = copy(effects = effs)
  object Player:
    def multSizeEffect(count : Int,fact : Double):Effect[Player] = 
      Effect(count,me => me.copy(path = me.path.copy(me.path.elementsp.map(fact * _))))
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