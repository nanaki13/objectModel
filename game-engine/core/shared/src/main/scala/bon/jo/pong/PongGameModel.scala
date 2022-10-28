package bon.jo
import bon.jo.common.Geom2D.*
import bon.jo.System.*
import bon.jo.pong.Shape
import bon.jo.common.typeutils.~
import bon.jo.common.Geom2D.Vector.*
import bon.jo.pong.Effect.ConditionalEffect
package pong:
  // type WithEffs[T] = T & Effects[T]
  trait Effects[T]:

    me: T =>
    val effects: Seq[Effect[T]]
    type EffOnMe = Effect[T]
    def withEffect(effs: Seq[EffOnMe]): T with Effects[T]
    def addEffect(e : EffOnMe): T with Effects[T] = withEffect(effects :+ e)
    def applyEffect(): T =
      if this.effects.nonEmpty then
        val res: (T with Effects[T], Seq[EffOnMe]) =
          this.effects.foldLeft(me -> Seq.empty[EffOnMe])((acc, eff) => {
            val (t, effs) = acc
            val (neff, nt) = eff(t)
            nt -> (effs :+ neff)
          })
        val (nMe, nEff) = res
        nMe.withEffect(nEff.filterNot(_.isFinish(nMe)))
      else this
  trait Effect[T]:
    def effect(t: T): T with Effects[T]
    def nextState(): Effect[T]

    def apply(t: T): (Effect[T], T with Effects[T]) =
      nextState() -> effect(t)

    def isFinish(t: T): Boolean
  trait CountEffect[T] extends Effect[T]:
    val count: Int
    def nextState(): Effect[T]
    def isFinish(t: T): Boolean = count <= 0

  object Effect:

    // def multEffect[T <: PosSpeed]( count : Int,f : (t : T&Effects[T],d : Double)=>WithEffs[T]):Effect[T] = apply()

    def countEffect[T](count: Int, effectf: T => T with Effects[T]): Effect[T] =
      ImplCountEffect(count, effectf)
    case class ImplCountEffect[T](count: Int, effectf: T => T with Effects[T])
        extends CountEffect[T]:
      inline def effect(t: T): T with Effects[T] = effectf(t)
      def nextState(): Effect[T] = copy(count - 1)
    abstract class ConditionalEffect[T] extends Effect[T]:
      def nextState(): Effect[T] = this
  enum Input:
    case Up, Down, Left, Right, No
  enum Gift(corner: Point, speed: Vector)
      extends Shape[Gift](
        ComputedPath(List(10 * up, 10 * right, 10 * down, 10 * left), corner),
        speed: Vector
      ):
    def withPosAndSpeed(pos: Point, speed: Vector): Gift =
      this match
        case NewBall(_, _)       => NewBall(pos, speed)
        case GreaterPlayer(_, _) => GreaterPlayer(pos, speed)
        case SmallerPlayer(_, _) => SmallerPlayer(pos, speed)
        case GreaterBall(_, _)   => GreaterBall(pos, speed)
        case SmallerBall(_, _)   => SmallerBall(pos, speed)
        case FasterBall(_, _)    => FasterBall(pos, speed)
        case SlowBall(_, _)      => SlowBall(pos, speed)
        case FasterPlayer(_, _)  => FasterPlayer(pos, speed)
        case SlowPlayer(_, _)    => SlowPlayer(pos, speed)

    case NewBall(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)

    case GreaterPlayer(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case SmallerPlayer(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case GreaterBall(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case SmallerBall(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case FasterBall(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case SlowBall(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case FasterPlayer(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
    case SlowPlayer(corner: Point, override val speed: Vector)
        extends Gift(corner: Point, speed: Vector)
  object Gift:
    def random(pos: Point, speed: Vector): Gift =
      val r = Math.random()
      if r < 1d / 9 then NewBall(pos, speed)
      else if r < 2d / 9 then GreaterPlayer(pos, speed)
      else if r < 3d / 9 then GreaterBall(pos, speed)
      else if r < 4d / 9 then SmallerPlayer(pos, speed)
      else if r < 5d / 9 then SmallerBall(pos, speed)
      else if r < 6d / 9 then FasterBall(pos, speed)
      else if r < 7d / 9 then FasterPlayer(pos, speed)
      else if r < 8d / 9 then SmallerBall(pos, speed)
      else SmallerPlayer(pos, speed)

      // GreaterPlayer(pos,speed)

  case class Board(paths: List[ComputedPath])
      extends Shape[Board](paths.head, Vector(0, 0))
      with SystemElement:
    val seg = paths.flatMap(_.segments)
    val minY = seg.map(s => Math.min(s.p1.y, s.p2.y)).min
    val maxY = seg.map(s => Math.max(s.p1.y, s.p2.y)).max
    val minX = seg.map(s => Math.min(s.p1.x, s.p2.x)).min
    val maxX = seg.map(s => Math.max(s.p1.x, s.p2.x)).max
    val h = maxY - minY
    val w = maxX - minX
    def withPosAndSpeed(pos: Point, speed: Vector): Board = copy(
      paths.map(_.copy(fromp = pos))
    )

  case class Ball(
      shape: DiscretCircle,
      speed: Vector,
      effects: Seq[Effect[Ball]]
  ) extends PosSpeed[Ball]
      with Effects[Ball]:

    def withEffect(effs: Seq[Effect[Ball]]): Ball = copy(effects = effs)
    def withPosAndSpeed(pos: Point, speed: Vector): Ball =
      copy(shape = shape.copy(center = pos), speed = speed, effects = effects)
    def pos: Point = shape.center
  object Ball:


    def multSizeEffect(count: Int, fact: Double): Effect[Ball] =
      Effect.countEffect(
        count,
        me => me.copy(shape = me.shape.copy(r = me.shape.r * fact))
      )

  abstract class Shape[T <: PosSpeed[T]](val valuep: ComputedPath, val speed: Vector)
      extends PosSpeed[T]:

    def cross(s: Segment): (Debug) ?=> Seq[Point] =
      valuep.segments.flatMap(_.cross(s))
    def pos: bon.jo.common.Geom2D.Point = valuep.from

  case class Rock(
      value: ComputedPath,
      override val speed: Vector = Vector(0, 0),
      color: String,
      gift: Option[Gift],
      life : Int
  ) extends Shape[Rock](value, speed):
    def withPosAndSpeed(pos: Point, speed: Vector): Rock =
      copy(value.copy(fromp = pos), speed)
  case class Player(
      path: ComputedPath,
      override val speed: Vector,
      speedPlayer: Double,
      maxSpeed: Double,
      dir: Vector,
      rosckTouch: Int,
      giftTouch: Int,
      effects: Seq[Effect[Player]] = Seq.empty
      
  ) extends Shape[Player](path, speed)
      with Effects[Player] with ScoreElements:
    inline def withPosAndSpeed(pos: Point, speed: Vector): Player =
      copy(path.copy(fromp = pos), speed)
    inline def withEffect(effs: Seq[Effect[Player]]): Player =
      copy(effects = effs)
    inline def withScore(rosckTouch: Int, giftTouch: Int): Player =
      copy(rosckTouch = rosckTouch, giftTouch = giftTouch)
    inline def addToScore(rosckTouchp: Int, giftTouchp: Int): Player =
      withScore(rosckTouch + rosckTouchp, giftTouch + giftTouchp)

  object Player:
    class AccSpped(speedMul: Double) extends ConditionalEffect[Player]:
      def effect(t: Player): Player =
        val nSpeedLength = speedMul * t.speedPlayer
        t.copy(speedPlayer = nSpeedLength, speed = nSpeedLength * t.dir)

      def isFinish(t: Player): Boolean =
        t.speedPlayer > t.maxSpeed
    def multSizeEffect(count: Int, fact: Double): Effect[Player] =
      Effect.countEffect(
        count,
        me => me.copy(path = me.path.copy(me.path.elementsp.map(fact * _)))
      )
    def multSpeedEffect(count: Int, fact: Double): Effect[Player] =
      Effect.countEffect(
        count,
        me => {
          me.copy(maxSpeed = fact * me.maxSpeed)
        }
      )

  trait PosSpeed[T <: PosSpeed[T]] extends SystemElement:
    def pos: Point
    def speed: Vector
    def withPosAndSpeed(pos: Point, speed: Vector): T
    def withPos(pos: Point): T = withPosAndSpeed(pos, speed)
    def withSpeed(speed: Vector): T = withPosAndSpeed(pos, speed)
    def move(): T =
      withPos(pos + speed)
  object PosSpeed:
    class DownToMySpeedEffect[T <: PosSpeed[T] & Effects[T]](speed : Double,f : Double => Double) extends ConditionalEffect[T]:
    
      override def effect(t: T): T & Effects[T] = 
        val cSpeed =  t.speed.length 
        if cSpeed< speed then
          t.withSpeed(speed = speed * t.speed.unitary() )
        else 
          var nSpeed = f(cSpeed)
          nSpeed = if nSpeed > speed then nSpeed else speed
          t.withSpeed(speed = nSpeed * t.speed.unitary() )


      override def isFinish(t: T): Boolean = t.speed.length <= speed
    def multSpeedEffect[U <: PosSpeed[U] with Effects[U]](
        count: Int,
        fact: Double
    ): Effect[U] =
      Effect.countEffect(
        count,
        me =>
          {
            me.withPosAndSpeed(pos = me.pos, speed = fact * me.speed)
          }.asInstanceOf
      )

  case class PongSystem(
      balls: Seq[Ball],
      player: Seq[Player],
      board: Board,
      rocks: Seq[Rock],
      gifts: Seq[Gift]
  ) extends System:
    def isGameOver(timeLeft: Long): GameOver =
      val giftTouch = player.map(e => e.giftTouch).sum
      val rosckTouch = player.map(e => e.rosckTouch).sum
      if rocks.isEmpty then GameOver.Victory(giftTouch, rosckTouch, timeLeft)
      else if balls.isEmpty then GameOver.Loose(giftTouch, rosckTouch)
      else GameOver.No
  enum GameOver:
    case Victory(giftTouch: Int, rosckTouch: Int, timeLeft: Long)
        extends GameOver
        with End
    case Loose(giftTouch: Int, rosckTouch: Int) extends GameOver with End
    case No
  case class PointCount(
      pointsByRock: Int,
      pointsByGift: Int,
      pointsBySecond: Int
  )
  inline def pointCount: ~[PointCount] = summon
  sealed trait ScoreElements:
    val giftTouch: Int
    val rosckTouch: Int
    def baseScore: PointCount ?=> Int = giftTouch * pointCount.pointsByGift + rosckTouch * pointCount.pointsByRock

  sealed trait End extends ScoreElements:
    def score: PointCount ?=> Int =
      val timeScore = this match
        case GameOver.Victory(_, _, timeLeft) =>
          (timeLeft / 1000d).round.toInt * pointCount.pointsBySecond
        case GameOver.Loose(_, _) => 0

      baseScore + timeScore

    // def elements: Seq[SystemElement] = balls ++ player ++ rocks
  // object PongSystem:
  // def apply( l : List[SystemElement],board : Board  ) : PongSystem=

  // PongSystem(l.head.asInstanceOf,l.tail.filter(_.isInstanceOf[Player]).asInstanceOf,board,l.tail.filter(_.isInstanceOf[Rock]).asInstanceOf)
