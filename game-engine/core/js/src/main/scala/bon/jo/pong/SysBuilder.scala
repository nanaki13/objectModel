package bon.jo.pong
import bon.jo.Geom2D.*
import bon.jo.Geom2D.Vector.*
import scala.util.Random
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
  inline def fact: SysParam ?=> Int = sysParam.fact
   def makeRocks(
      width: Double,
      height: Double,
      blockByRow: Int,
      blockByColumn: Int,
      from: Point
  ): Seq[Rock] =
    val widthBlock = width / blockByRow
    val heightBlock = height / blockByColumn
    val fromV = Vector(from)
    for {
      xi <- 0 until blockByRow
      yi <- 0 until blockByColumn
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
        None
      )
    } yield r
  inline def makeRocks(
      board: Board,
      blockByRow: Int,
      blockByColumn: Int
  ): SysParam ?=> Seq[Rock] =
    val minw = board.paths
      .flatMap(_.segments.flatMap(z => List(z.p1, z.p2)))
      .map(_.x)
      .min
    makeRocks(
      board.w - 10 * fact,
      board.h * 0.5,
      blockByRow,
      blockByColumn,
      Point(minw + 5 * fact, board.h * 0.1)
    )

  def createSys(fact: Int) =
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
      i <- 1 until 8
      d = i * teta

    } yield 4 * fact * (down rotate d)

    // ++ List(25* fact * right,5* fact * down,25* fact * left,5* fact * up)
    val base =
      3 * fact * down +: path.map(v => Vector(v.x, v.y / 2)) :+ 3 * fact * up
    val tot = base.reduce(_ + _)
    val pathPlayer = ComputedPath(
      base :+ (-tot),
      Point(75 * fact, 203 * fact)
    ) // .biso(2* fact)

    def r1 = Random.nextInt(10) * 12 * fact
    def r2 = Random.nextInt(10) * 20 * fact
    def point = Point(10 + r1, 50 + r2)
    val rocks =
      // for(_ <- 0 until 50 ) yield Rock(ComputedPath(List(5 * fact* up,10 * fact* right,5* fact * down,10* fact * left),point).biso(2* fact))
      //makeRocks(board, 7, 15).map(_.giftRandom)
      makeRocks(board, 2, 2).map(_.giftRandom)
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
