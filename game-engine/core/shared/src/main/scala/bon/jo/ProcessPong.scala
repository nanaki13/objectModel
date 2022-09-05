package bon.jo.pong


import bon.jo.Geom2D.*
import bon.jo.Geom2D.Vector.`*`
import bon.jo.System
import bon.jo.System.*
trait Debug:
   def debug():Unit
   def debug(s  :String):Unit = 
      println(s)
      debug()
object Debug : 
   inline def apply(s  :String):Debug ?=>Unit = summon.debug(s)
   inline def apply():Debug ?=>Unit = summon.debug()
class ProcessPong[C](using Debug,Drawer[C],C) extends SystemProcess[PongSystem]:

   type PongSys[A] = Sys[PongSystem,(Ball,Set[Rock])]

   def next(): SystemFlow[PongSystem] = 
      val sys = System()
      val balls = sys.balls
      val board : Board = sys.board
      val b =  balls.map(process)
      val ballsMod = b.map(_._1)
      val goodBall  = ballsMod.filter{
         b => b.pos.x < board.maxX +10 && b.pos.x > board.minX - 10 && 
            b.pos.y < board.maxY +10 && b.pos.y > board.minY
      }
      val rocksToRem = b.flatMap(_._2)
      val newBalls = 
         if rocksToRem.nonEmpty && Math.random()>0.95 then
            val rockToBall : Map[Rock,Seq[Ball]] = b.flatMap{( el:(Ball, Set[Rock])) => el._2.map(e => (e,el._1))}.groupMap(_._1)(_._2)
            rockToBall.headOption.map{
               (r,bs )=> bs.head.copy(speed = -bs.head.speed)
            }
         else
            Nil
      
      sys.copy(balls = goodBall++newBalls,player = sys.player.map(_.move[Player]()),rocks = sys.rocks.filter(e => !rocksToRem.contains(e)))
      //println(s.asInstanceOf[Ball].pos)

  

   def process( b : Ball):PongSys[(Ball,Set[Rock])] =
      val speedN = b.speed.length
      var dist = 0d
      var tmpE = b
      var crosCount = 0
      val sys = System()
      val board : Board = sys.board
      var rSet  :Set[Rock] = Set.empty
      val segs :List[(SystemElement,Segment)]= board.paths.flatMap(e =>  e.segments.map(board -> _)) ++ sys.player.flatMap(p => p.path.segments.map( p -> _)) ++ 
         sys.rocks.flatMap(r => r.value.segments.map(r -> _))
      val poinImageSources = 
         for
            p <- b.shape.points()
            (el,seg) <- segs
            ptoNew = p -> b.speed
            cross <- ptoNew.cross(seg)
            newSym = ptoNew.p2.sym(seg)   
         yield
            (Segment(cross,newSym),el,p)
      for
         el <- poinImageSources.map(_._2)
      yield
         rSet = el match
               case r : Rock => rSet + r
               case o => rSet 
      val l = poinImageSources.map(_._1.p2)
      if l.nonEmpty then
         val somme = poinImageSources.map(_._1.toVector()).reduce(_ + _)
         
         val nv = 
            if somme != Point(0,0) then
               speedN * somme.unitary()
            else
               - b.speed
         (b.copy(b.pos + nv,nv),rSet)
      else
         (b.copy( b.pos + b.speed, b.speed),rSet)  
      
      /*

      while(dist < speedN && crosCount <99){
         val seg = tmpE.pos -> (1-dist/speedN) * tmpE.speed 
         
        // val ord : Ordering[(SystemElement,Point,Double)]= (a,b) => a._3.compare(b._3)
         val firstCol = segs.map{
             (source,sB) =>  (source,sB,sB.cross(seg)) 
         }.filter(_._3.isDefined).map(e=>(e._1,e._2,e._3.get,(e._3.get --> b.pos).length)).sortBy(_._4)
         
         val crosss = firstCol.headOption.flatMap{
         (source,sB,i,d) => 
            
            
            rSet = source match
               case r : Rock => rSet + r
               case o => rSet
            val sym = seg.p2.sym(sB)
            crosCount+=1
            val dri = Segment(i,sym).toVector().unitary()
            
            if dri.x.toString != "NaN" && dri.y.toString != "NaN" then      
               Some(tmpE.copy( sym,(b.speed.length *dri)))
            else 
               None
               
         }
         
         
         crosss.headOption match
            case Some(v) => 
               dist = Vector(v.pos,tmpE.pos).length + dist
               tmpE = v
            case o => 
               tmpE = tmpE.copy(tmpE.pos + (1-dist/speedN) * tmpE.speed,tmpE.speed )
               dist = speedN
      }
      (tmpE,rSet) */
            
     
