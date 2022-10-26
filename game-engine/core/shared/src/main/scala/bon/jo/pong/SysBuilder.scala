package bon.jo.pong
import bon.jo.common.Geom2D.*
import bon.jo.common.Geom2D.Vector.*
import scala.util.Random
import scala.reflect.ClassTag
object SysBuilder:
  extension (r : Rock)
    def giftRandom:Rock = 
        val g  =if Math.random() > 0 then
          Some(Gift.random(r.value.middle(),Vector(0,3)))
        else None
        r.copy(gift = g)
  def randomHSL() =
    val r = Random()
    s"hsl(${r.nextDouble * 360}deg ${r.nextDouble * 100}% ${r.nextDouble * 100}%)"
  case class SysParam(fact: Int)
  inline def sysParam: SysParam ?=> SysParam = summon
  class RockMatrice(val w:Int,val h:Int, val matrice : scala.collection.mutable.Map[(Int,Int),Int] = scala.collection.mutable.Map()) extends PosMatrice {

    override def life(x: Int, y: Int): Int = matrice(x -> y)

  }
  sealed trait PosMatrice:
    def life(x : Int, y : Int): Int
  object AllPosMatrice extends PosMatrice:
    override def life(x: Int, y: Int): Int = 1
   
  object RockMatrice:
    def apply(tab : Array[Array[Int]]) : RockMatrice = 
      val mat = new RockMatrice(tab(0).length,tab.length)
      for{
        (values,y) <- tab.zipWithIndex
        (value,x)<- values.zipWithIndex
      } {
        mat.matrice += (x, y)-> value
      }
      mat
  inline def fact: SysParam ?=> Int = sysParam.fact
   def makeRocks(
      width: Double,
      height: Double,
      from: Point
  )(using matrice :RockMatrice): Seq[Rock] =
    val widthBlock = width / matrice.w
    val heightBlock = height / matrice.h
    val fromV = Vector(from)
   
    for {
      xi <- 0 until  matrice.w
      yi <- 0 until  matrice.h
      if matrice.life(xi,yi) != 0
      p = Point(widthBlock * xi, heightBlock * yi)
      r = Rock(
        ComputedPath(
          List(
            heightBlock * up,
            widthBlock * right,
            heightBlock * down,
            widthBlock * left
          ),
          p + fromV
        ),
        Vector(0, 0),
        randomHSL(),
        None, matrice.life(xi,yi)
      )

    } yield r
  inline def makeRocks(
      board: Board
  ): (RockMatrice, SysParam) ?=> Seq[Rock] =
    val minw = board.paths
      .flatMap(_.segments.flatMap(z => List(z.p1, z.p2)))
      .map(_.x)
      .min
    makeRocks(
      board.w - 10 * fact,
      board.h * 0.7,
      Point(minw + 5 * fact, board.h * 0.1)
    )

  inline def |<[T : ClassTag](t : T *) : Array[T] = Array(t *)
  def matLvel1 : Array[Array[Int]] = |<(
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0),
    |<(0,1,0,1,0,1,0)
  )
  def matLvel2 : Array[Array[Int]] = |<(
    |<(0,0,0,1,0,0,0,0,0,0,1,0,0,0),
    |<(0,1,1,1,1,1,1,1,1,1,1,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,0,1,1,0,0,1,1,0,1,1,0),
    |<(0,1,0,0,0,0,0,0,0,0,0,0,1,0),
    |<(0,1,0,1,0,0,0,0,0,0,1,0,1,0),
    |<(0,1,1,3,1,1,0,0,1,1,3,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,0,1,1,0,0,1,1,0,1,1,0),
    |<(0,1,0,0,0,0,0,0,0,0,0,0,1,0),
    |<(0,1,0,1,0,0,0,0,0,0,1,0,1,0),
    |<(0,1,1,3,1,1,0,0,1,1,3,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,1,1,1,1,1,1,1,1,1,1,0),
    |<(0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    |<(0,1,1,3,1,1,0,0,1,1,3,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,1,1,1,1,1,1,1,1,1,1,0),
     |<(0,1,1,1,1,1,1,1,1,1,1,1,1,0),
    |<(0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    |<(0,1,1,3,1,1,0,0,1,1,3,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    |<(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
    def matLvel3 : Array[Array[Int]] = |<(
    |<(0,0,0,1,0,0,0,0,0,0,1,0,0,0),
    |<(0,1,1,1,1,1,1,1,1,1,1,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,0,1,1,0,0,1,1,0,1,1,0),
    |<(0,1,0,0,0,0,0,0,0,0,0,0,1,0),
    |<(0,1,0,1,0,0,0,0,0,0,1,0,1,0),
    |<(0,1,1,3,1,1,0,0,1,1,3,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,0,1,1,0,0,1,1,0,1,1,0),
    |<(0,1,0,0,0,0,0,0,0,0,0,0,1,0),
    |<(0,1,0,1,0,0,0,0,0,0,1,0,1,0),
    |<(0,1,1,3,1,1,0,0,1,1,3,1,1,0),
    |<(0,1,0,0,0,1,0,0,1,0,0,0,1,0),
    |<(0,1,1,1,1,1,1,1,1,1,1,1,1,0),
    |<(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  )
    def matLvel4 : Array[Array[Int]] = |<(
    |<(0,0,0,1,0,0,0),
    |<(0,2,1,2,1,2,0),
    |<(1,1,2,3,2,1,1),
    |<(0,1,1,2,1,1,0),
    |<(0,0,0,1,0,0,0),
    |<(0,0,0,1,0,0,0),
    |<(0,2,1,2,1,2,0),
    |<(1,1,2,3,2,1,1),
    |<(0,1,1,2,1,1,0),
    |<(0,0,0,1,0,0,0),
    |<(0,0,0,1,0,0,0),
    |<(0,2,1,2,1,2,0),
    |<(1,1,2,3,2,1,1),
    |<(0,1,1,2,1,1,0),
    |<(0,0,0,1,0,0,0)
  )
  def createSys(fact: Int):PongSystem =
    given SysParam = SysParam(fact)
    val board = Board(
      List(
        ComputedPath(
          List(
            10 * fact * left,
            200 * fact * down,
            150 * fact * right,
            200 * fact * up,
            10 * fact * left,
            5 * fact * up,
            15 * fact * right,
            210 * fact * down,
            160 * fact * left,
            210 * fact * up,
            15 * fact * right,
            5 * fact * down
          ),
          Point(15 * fact, 205 * fact)
        )
      )
    )
    val teta = Math.PI / 8
    val path = for {
      i <- 1 until 16
      d = i * teta

    } yield 4 * fact * (down rotate d)

    // ++ List(25* fact * right,5* fact * down,25* fact * left,5* fact * up)
    val base =
       path.map(v => Vector(v.x, v.y / 2)) 
  //  val tot = base.reduce(_ + _)
    val pathPlayer = ComputedPath(
      base,
      Point(75 * fact, 203 * fact)
    ) // .biso(2* fact)

    def r1 = Random.nextInt(10) * 12 * fact
    def r2 = Random.nextInt(10) * 20 * fact
    def point = Point(10 + r1, 50 + r2)
    given RockMatrice = RockMatrice(matLvel2)
    val rocks =
      // for(_ <- 0 until 50 ) yield Rock(ComputedPath(List(5 * fact* up,10 * fact* right,5* fact * down,10* fact * left),point).biso(2* fact))
      makeRocks(board).map(_.giftRandom)
     // makeRocks(board, 10, 1).map(_.giftRandom)
    PongSystem(
      Ball(
        DiscretCircle(3 * fact, Point(board.w / 2, board.h - 10 * 10), 8),
        Vector(5, -5),
        Seq()
      ) :: Nil,
      Player(pathPlayer, Vector(0, 0), 0.2, 3 * fact, Vector(0, 0),0,0) :: Nil,
      board,
      rocks.toList,
      Seq.empty
    )
