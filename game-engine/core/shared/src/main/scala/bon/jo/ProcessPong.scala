package bon.jo

import bon.jo.pong.PongSystem
import bon.jo.pong.Ball
import bon.jo.pong.PosSpeed
import bon.jo.pong.Player
import bon.jo.Geom2D.*
import bon.jo.pong.Board
import bon.jo.pong.Drawer
import bon.jo.pong.Rock
trait Debug:
   def debug():Unit
   def debug(s  :String):Unit = 
      println(s)
      debug()
class ProcessPong[C](using Debug,Drawer[C],C) extends SystemProcess[PongSystem]:

    def next(): SystemFlow[PongSystem] = 
      val sys = System()
      val ball = sys.ball
      val board : Board = sys.board
      //println(s.asInstanceOf[Ball].pos)


      def process( b : Ball):(Ball,Set[Rock]) =
         val speedN = b.speed.length
         var dist = 0d
         var tmpE = b
         var crosCount = 0
         var rSet  :Set[Rock] = Set.empty
         while(dist < speedN && crosCount <99){
            val seg = tmpE.pos -> (1-dist/speedN) * tmpE.speed 
            val segs :List[(bon.jo.SystemElement,Segment)]= board.paths.flatMap(e =>  e.segments.map(board -> _)) ++ sys.player.flatMap(p => p.path.segments.map( p -> _)) ++ 
            sys.rocks.flatMap(r => r.value.segments.map(r -> _))
            val crosss = segs.flatMap{
            (source,sB) => 
               
               
               
               sB.cross(seg).map{
                  i =>
                     rSet = source match
                        case r : Rock => rSet + r
                        case o => rSet
                     val sym = seg.p2.sym(sB)
                     crosCount+=1
                    
                     tmpE.copy( sym,(b.speed.length * Segment(i,sym).toVector().unitary()))
                  
               }
            }
            crosss.headOption match
               case Some(v) => 
                  dist = Vector(v.pos,tmpE.pos).length + dist
                  tmpE = v
               case o => 
                  tmpE = tmpE.copy(tmpE.pos + (1-dist/speedN) * tmpE.speed,tmpE.speed )
                  dist = speedN
         }
         (tmpE,rSet)
            
      val(b,rockRemove) =   process(sys.ball)
      println(rockRemove)
      sys.copy(ball = b,player = sys.player.map(_.move[Player]()),rocks = sys.rocks.filter(e => !rockRemove.contains(e)))
