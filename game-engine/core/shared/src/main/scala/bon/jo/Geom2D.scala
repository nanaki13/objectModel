package bon.jo

import scala.annotation.tailrec
import Geom2D.Vector.*
import bon.jo.pong.Drawer
object Geom2D:

  case class Point(x : Double,y : Double)
  case class Segment(p1 : Point,p2 : Point):
    def reverse :Segment = Segment(p2,p1)
    def cross[C](cd : Segment):(Debug,Drawer[C],C) ?=> Option[Point] = 
      val draw = summon[Drawer[C]]
      import draw.*
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
      

     


     

      if (ab_v_cd != 0 && ab_v_ad * ab_v_ac <= 0 && cd_v_cb * cd_v_ca <= 0 ) then
        val s1_x = ab.p2.x - ab.p1.x
        val s1_y =  ab.p2.y - ab.p1.y
        val s2_x = cd.p2.x - cd.p1.x
        val s2_y = cd.p2.y - cd.p1.y
        val s = (-s1_y * (ab.p1.x - ab.p2.x) + s1_x * (ab.p1.y - ab.p2.y)) / (-s2_x * s1_y + s1_x * s2_y)
        val t = ( s2_x * (ab.p1.y - cd.p1.y) - s2_y * (ab.p1.x - cd.p1.x)) / (-s2_x * s1_y + s1_x * s2_y)
        //summon[Debug].debug("Inter !")
        Some(Point(ab.p1.x + (t * s1_x),ab.p1.y + (t * s1_y)))
      else
        None
   

    def toVector():Vector = p2 - p1
  trait PosSpeed :
    def pos  : Point
    def speed  : Vector
    def impact[C](s : Segment):(Debug,Drawer[C],C) ?=> Option[Point] = 
      Segment(pos,pos + speed).cross(s)
  sealed abstract class Path(elements : List[Vector],from : Point = O):
    def segments :  List[Segment]= toSegments()
    def join(p : Path):ComputedPath = 
      val nSeg = (segments :+ Segment(segments.last.p2,p.segments.head.p1))++p.segments
      ComputedPath(nSeg.map(_.toVector()),from)
    def reverse():ComputedPath = 
      val seg = segments.map(_.reverse).reverse
      ComputedPath(seg.map(_.toVector()),seg.head.p1)
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
    override val segments = toSegments()
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
      def unitary():Vector = ( 1 / length) * p
      
    
      inline def x : Double = p.x
      inline def y : Double = p.y
      inline def segFrom(o : Point):Segment = Segment(p,p+o)
      def length : Double = Math.sqrt(p.x * p.x + p.y * p.y)
  extension (p : Point)
    def sym(seg  :Segment):Point = 
      val u = seg.toVector().unitary()
      var scal = Vector(seg.p1,p) * u
      scal = if scal < 0 then -scal else scal
      val h = seg.p1 + (scal * u)
      val ph = Vector(p,h)
      h + ph
    def +(o :  Vector):Point = Point(p.x+o.x,p.y+o.y)
    def -(o :  Vector):Point = Point(p.x-o.x,p.y-o.y)
    inline def ->(o :  Vector):Segment = Segment(p,p+o)
    
    