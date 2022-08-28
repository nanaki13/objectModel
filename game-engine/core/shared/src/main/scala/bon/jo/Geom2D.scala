package bon.jo

import scala.annotation.tailrec
import Geom2D.Vector.*
object Geom2D:

  case class Point(x : Double,y : Double)
  case class Segment(p1 : Point,p2 : Point):
    def cross(cd : Segment):Boolean = 
      val ab = this
      val abV  = ab.toVector()
      val cdV  = cd.toVector()
      val ac =Vector(ab.p1,cd.p1)
      val ca = - ac
      val ad =Vector(ab.p1,cd.p2)
      val cb =Vector(cd.p1,ab.p2)
      val ab_v_cd = abV ^ cdV
      val ab_v_ad = abV ^ ad
      val ab_v_ac = abV ^ ac
      val cd_v_cb = cdV ^ cb
      val cd_v_ca = cdV ^ ca
      ab_v_cd != 0 && ab_v_ad * ab_v_ac <= 0 && cd_v_cb * cd_v_ca <= 0 
    def toVector():Vector = p2 - p1
  trait PosSpeed :
    def pos  : Point
    def speed  : Vector
    def impact(s : Segment):Boolean = 
      Segment(pos,pos + speed).cross(s)
  sealed abstract class Path(elements : List[Vector],from : Point = O):
    def toSegments():List[Segment] = 
      computeSegments(from,from + elements.head,elements.tail,Nil)

    @tailrec
    private def computeSegments(p1 : Point,p2 : Point,still : List[Vector],done : List[Segment]):List[Segment] = 
      if still.isEmpty then
        done :+ Segment(p1,p2) 
      else
        computeSegments(p2,p2 + still.head,still.tail,done:+ Segment(p1,p2))
  case class LazyPath(elements : List[Vector],from : Point = O) extends Path(elements,from)
  case class ComputedPath(elements : List[Vector],from : Point = O) extends Path(elements,from):
    val segments = toSegments()
    val h = {
      val seg = segments
      val min = seg.map(s => Math.min(s.p1.y,s.p2.y)).min
      val max = seg.map(s => Math.max(s.p1.y,s.p2.y)).max
      max - min
    }
    val w = {
      val seg = segments
      val min = seg.map(s => Math.min(s.p1.x,s.p2.x)).min
      val max = seg.map(s => Math.max(s.p1.x,s.p2.x)).max
      max - min
    }
    println((w,h))
  object O extends Point(0 : Double,0 : Double)
  opaque type Vector = Point
  object Vector : 
    inline def apply(x : Double,y : Double):Vector =Point(x ,y )
    def apply(p1 : Point,p2 : Point):Vector = p2 - p1
    inline def apply(p1 : Point):Vector = p1
    val left = Vector(-1,0)
    val right =  Vector(1,0)
    val up =  Vector(0,1)
    val down = Vector(0,-1)
    extension (p : Double )
      def *(o : Vector):Vector =  Point(p*o.x,p*o.y)
    extension (p : Vector)
      def unary_- : Vector = Point(- p.x,- p.y)
      def *(o : Vector):Double = p.x * o.x + p.y * o .y
      def ^(o : Vector):Double = p.x * o.y - p.y * o .x
      inline def x : Double = p.x
      inline def y : Double = p.y
  extension (p : Point)
    def +(o :  Vector):Point = Point(p.x+o.x,p.y+o.y)
    def -(o :  Vector):Point = Point(p.x-o.x,p.y-o.y)
    def length : Double = Math.sqrt(p.x * p.x + p.y * p.y)
    